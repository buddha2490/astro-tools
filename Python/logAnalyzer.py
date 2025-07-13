import re
import sys
from datetime import datetime
from collections import defaultdict

class NINALogAnalyzer:
    def __init__(self):
        self.event_patterns = {
            'image_capture': {
                'start': r'Starting Exposure - Exposure Time:',
                'end_next_event': True  # Ends when next different event starts
            },
            'plate_solving': {
                'start': r'Platesolving with parameters',
                'end': r'Platesolve (successful|failed)'
            },
            'filter_wheel': {
                'start': r'Starting Category: Filter Wheel, Item: SwitchFilter',
                'end': r'Finishing Category: Filter Wheel, Item: SwitchFilter'
            },
            'autofocus': {
                'start': r'Starting Category: Focuser, Item: RunAutofocus',
                'end': r'Finishing Category: Focuser, Item: RunAutofocus'
            },
            'meridian_flip': {
                'start': r'Meridian Flip.*initiated|Starting.*meridian.*flip',
                'end': r'Meridian Flip.*complete|Finishing.*meridian.*flip'
            },
            'waiting_dithering': {
                'start': r'Phd2.*start.*guiding|dithering|settling',
                'end': r'guiding.*started|dithering.*complete|settling.*complete'
            }
        }
    
    def parse_timestamp(self, timestamp_str):
        """Parse NINA timestamp format: 2025-07-07T22:45:04.8818"""
        # Remove any trailing pipe characters
        clean_timestamp = timestamp_str.split('|')[0]
        
        # Handle the 4-digit milliseconds by padding to 6 digits for microseconds
        if '.' in clean_timestamp:
            date_part, fractional_part = clean_timestamp.split('.')
            # Pad fractional seconds to 6 digits (microseconds)
            fractional_part = fractional_part.ljust(6, '0')[:6]
            clean_timestamp = f"{date_part}.{fractional_part}"
        
        return datetime.fromisoformat(clean_timestamp)
    
    def find_sequences(self, log_lines):
        """Find all advanced sequences in the log"""
        sequences = []
        current_start = None
        
        for i, line in enumerate(log_lines):
            if 'Advanced Sequence starting' in line:
                if current_start is not None:
                    # Previous sequence ended abruptly, close it
                    sequences.append((current_start, i-1))
                current_start = i
            elif current_start is not None and ('sequence.*complete' in line.lower() or 
                                               'sequence.*finished' in line.lower() or
                                               i == len(log_lines) - 1):
                sequences.append((current_start, i))
                current_start = None
        
        return sequences
    
    def extract_events(self, log_lines, start_idx, end_idx):
        """Extract events within a sequence"""
        events = []
        all_log_entries = []  # Store all log entries for untracked analysis
        active_events = {}  # Track currently active events
        
        for i in range(start_idx, end_idx + 1):
            line = log_lines[i]
            timestamp_match = re.match(r'(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d+)', line)
            if not timestamp_match:
                continue
                
            try:
                timestamp = self.parse_timestamp(timestamp_match.group(1))
            except ValueError as e:
                print(f"Warning: Could not parse timestamp '{timestamp_match.group(1)}': {e}")
                continue
            
            # Store all log entries for untracked analysis
            all_log_entries.append((timestamp, line.strip()))
            
            # Check for event starts
            for event_type, patterns in self.event_patterns.items():
                if re.search(patterns['start'], line, re.IGNORECASE):
                    # Special case: if image capture starts and dithering is active, end dithering
                    if event_type == 'image_capture' and 'waiting_dithering' in active_events:
                        start_time = active_events.pop('waiting_dithering')
                        duration = (timestamp - start_time).total_seconds()
                        events.append(('waiting_dithering', start_time, timestamp, duration))
                    
                    active_events[event_type] = timestamp
                    break
            
            # Check for event ends
            for event_type, patterns in self.event_patterns.items():
                if event_type in active_events:
                    if 'end' in patterns and re.search(patterns['end'], line, re.IGNORECASE):
                        start_time = active_events.pop(event_type)
                        duration = (timestamp - start_time).total_seconds()
                        events.append((event_type, start_time, timestamp, duration))
                    elif patterns.get('end_next_event') and event_type in active_events:
                        # For image capture, check if a different event type starts
                        for other_type, other_patterns in self.event_patterns.items():
                            if other_type != event_type and re.search(other_patterns['start'], line, re.IGNORECASE):
                                start_time = active_events.pop(event_type)
                                duration = (timestamp - start_time).total_seconds()
                                events.append((event_type, start_time, timestamp, duration))
                                break
        
        # Close any remaining active events
        if log_lines and end_idx < len(log_lines):
            final_line = log_lines[end_idx]
            final_match = re.match(r'(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d+)', final_line)
            if final_match:
                try:
                    final_timestamp = self.parse_timestamp(final_match.group(1))
                    for event_type, start_time in active_events.items():
                        duration = (final_timestamp - start_time).total_seconds()
                        events.append((event_type, start_time, final_timestamp, duration))
                except ValueError:
                    pass  # Skip if timestamp parsing fails
        
        return events, all_log_entries
    
    def merge_overlapping_events(self, events):
        """Merge overlapping events by priority"""
        # Priority order (higher priority events take precedence)
        priority = {
            'meridian_flip': 6,
            'autofocus': 5,
            'plate_solving': 4,
            'filter_wheel': 3,
            'waiting_dithering': 2,
            'image_capture': 1
        }
        
        merged_durations = defaultdict(float)
        time_slots = []
        
        # Create time slots for each second
        if not events:
            return merged_durations
            
        min_time = min(event[1] for event in events)
        max_time = max(event[2] for event in events)
        total_seconds = int((max_time - min_time).total_seconds()) + 1
        
        # Initialize time slots
        for second in range(total_seconds):
            time_slots.append(None)
        
        # Assign events to time slots based on priority
        for event_type, start_time, end_time, duration in events:
            start_second = int((start_time - min_time).total_seconds())
            end_second = int((end_time - min_time).total_seconds())
            
            for second in range(start_second, min(end_second + 1, total_seconds)):
                current_event = time_slots[second]
                if current_event is None or priority[event_type] > priority[current_event]:
                    time_slots[second] = event_type
        
        # Count seconds for each event type
        for event_type in time_slots:
            if event_type:
                merged_durations[event_type] += 1.0
        
        return merged_durations
    
    def find_untracked_events(self, all_log_entries, tracked_events):
        """Find events that occur during untracked time periods"""
        if not all_log_entries:
            return []
        
        # Create time intervals for tracked events
        tracked_intervals = []
        for event_type, start_time, end_time, duration in tracked_events:
            tracked_intervals.append((start_time, end_time))
        
        # Sort intervals by start time
        tracked_intervals.sort()
        
        # Find untracked log entries
        untracked_entries = []
        for timestamp, log_line in all_log_entries:
            is_tracked = False
            for start_time, end_time in tracked_intervals:
                if start_time <= timestamp <= end_time:
                    is_tracked = True
                    break
            
            if not is_tracked:
                # Clean up the log line to show just the relevant part
                clean_line = log_line.split('|', 2)[-1].strip() if '|' in log_line else log_line
                # Skip overly verbose or repetitive entries
                if (len(clean_line) > 10 and 
                    not re.search(r'(verbose|debug|trace)', clean_line, re.IGNORECASE) and
                    not clean_line.startswith('2025-')):
                    untracked_entries.append((timestamp, clean_line))
        
        return untracked_entries
    
    def analyze_log(self, log_file_path):
        """Main analysis function"""
        with open(log_file_path, 'r', encoding='utf-8') as file:
            log_lines = file.readlines()
        
        sequences = self.find_sequences(log_lines)
        
        if not sequences:
            print("No advanced sequences found in the log file.")
            return
        
        for seq_num, (start_idx, end_idx) in enumerate(sequences, 1):
            print(f"\n{'='*60}")
            print(f"SEQUENCE {seq_num} ANALYSIS")
            print(f"{'='*60}")
            
            # Extract sequence timeframe
            start_line = log_lines[start_idx]
            end_line = log_lines[end_idx]
            
            start_match = re.match(r'(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d+)', start_line)
            end_match = re.match(r'(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d+)', end_line)
            
            if not start_match or not end_match:
                print("Could not parse sequence timestamps.")
                continue
            
            try:
                start_time = self.parse_timestamp(start_match.group(1))
                end_time = self.parse_timestamp(end_match.group(1))
            except ValueError as e:
                print(f"Error parsing sequence timestamps: {e}")
                continue
            
            total_duration = (end_time - start_time).total_seconds()
            
            print(f"Start: {start_time.strftime('%Y-%m-%d %H:%M:%S')}")
            print(f"End:   {end_time.strftime('%Y-%m-%d %H:%M:%S')}")
            print(f"Total Duration: {int(total_duration)} seconds ({total_duration/3600:.1f} hours)")
            
            # Extract and analyze events
            events, all_log_entries = self.extract_events(log_lines, start_idx, end_idx)
            
            # Calculate individual event durations (no merging)
            event_durations = defaultdict(float)
            for event_type, start_time, end_time, duration in events:
                event_durations[event_type] += duration
            
            if not event_durations:
                print("No tracked events found in this sequence.")
                continue
            
            print(f"\n{'Event Type':<20} {'Duration (s)':<12} {'Percentage':<10}")
            print('-' * 45)
            
            # Sort by duration (descending)
            sorted_events = sorted(event_durations.items(), key=lambda x: x[1], reverse=True)
            
            for event_type, duration in sorted_events:
                percentage = (duration / total_duration) * 100
                print(f"{event_type.replace('_', ' ').title():<20} {int(duration):<12} {percentage:.1f}%")
            
            # Summary
            tracked_time = sum(event_durations.values())
            untracked_time = total_duration - tracked_time
            untracked_percentage = (untracked_time / total_duration) * 100
            
            print('-' * 45)
            print(f"{'Tracked Time':<20} {int(tracked_time):<12} {(tracked_time/total_duration)*100:.1f}%")
            print(f"{'Other/Untracked':<20} {int(untracked_time):<12} {untracked_percentage:.1f}%")
            
            # Show untracked events if there are any
            if untracked_time > 5:  # Only show if significant untracked time
                untracked_entries = self.find_untracked_events(all_log_entries, events)
                if untracked_entries:
                    print(f"\nOTHER/UNTRACKED EVENTS ({len(untracked_entries)} entries):")
                    print('-' * 60)
                    # Group similar events
                    event_summary = defaultdict(int)
                    for _, log_line in untracked_entries:
                        # Extract key activity from log line
                        activity = self.categorize_log_entry(log_line)
                        event_summary[activity] += 1
                    
                    for activity, count in sorted(event_summary.items(), key=lambda x: x[1], reverse=True):
                        if count > 2:  # Only show recurring activities
                            print(f"  {activity}: {count} entries")
    
    def categorize_log_entry(self, log_line):
        """Categorize a log entry into a general activity type"""
        log_lower = log_line.lower()
        
        # Common patterns to categorize untracked events
        if any(word in log_lower for word in ['equipment', 'camera', 'mount', 'scope']):
            return "Equipment Status"
        elif any(word in log_lower for word in ['target', 'coordinate', 'position']):
            return "Target/Position Updates"  
        elif any(word in log_lower for word in ['temperature', 'cooling', 'temp']):
            return "Temperature Control"
        elif any(word in log_lower for word in ['sequence', 'instruction', 'container']):
            return "Sequence Management"
        elif any(word in log_lower for word in ['safety', 'weather', 'cloud']):
            return "Safety/Weather Monitoring"
        elif any(word in log_lower for word in ['notification', 'info', 'status']):
            return "Status Updates"
        else:
            # Use first meaningful word
            words = log_line.split()
            for word in words:
                if len(word) > 4 and not word.startswith('2025'):
                    return f"Other ({word})"
            return "Other Activities"

def print_usage():
    """Print usage instructions"""
    print("NINA Log Analyzer")
    print("================")
    print("Usage: python nina_analyzer.py <log_file_path>")
    print("")
    print("Examples:")
    print("  python nina_analyzer.py nina_log.txt")
    print("  python nina_analyzer.py /path/to/your/logfile.log")
    print("  python nina_analyzer.py \"C:\\Users\\YourName\\Documents\\NINA Logs\\session.log\"")

# Main execution
if __name__ == "__main__":
    if len(sys.argv) != 2:
        print_usage()
        sys.exit(1)
    
    log_file_path = sys.argv[1]
    analyzer = NINALogAnalyzer()
    
    try:
        print(f"Analyzing log file: {log_file_path}")
        analyzer.analyze_log(log_file_path)
    except FileNotFoundError:
        print(f"Error: Could not find log file '{log_file_path}'")
        print("Please ensure the file path is correct and the file exists.")
        sys.exit(1)
    except Exception as e:
        print(f"Error analyzing log: {e}")
        sys.exit(1)