import type { Config } from "tailwindcss";

export default {
  content: ["./src/**/*.{ts,tsx}"],
  theme: {
    extend: {
      colors: {
        // Deep-space palette
        space: "#060912", // deepest background
        spaceTop: "#0b1024", // gradient origin (upper)
        surface: "#0f1626", // card / panel base
        surfaceAlt: "#0b1120", // alternating rows
        glass: "rgba(20, 28, 48, 0.55)", // glassmorphic fill
        border: "#1e2840",
        borderGlow: "#33436b",
        fg: "#e8edf9",
        muted: "#8a96b4",
        faint: "#5a6688",
        accent: "#7aa2ff", // starlight blue
        accentBright: "#a9c4ff",
        nebula: "#b478ff", // violet glow
        nebulaPink: "#ff7ab8",
      },
      fontFamily: {
        sans: ["Inter", "system-ui", "sans-serif"],
      },
      boxShadow: {
        glow: "0 0 24px -4px rgba(122, 162, 255, 0.45)",
        glowSoft: "0 0 40px -8px rgba(122, 162, 255, 0.25)",
        card: "0 8px 30px -12px rgba(0, 0, 0, 0.6)",
      },
      backgroundImage: {
        "nebula-radial":
          "radial-gradient(900px 600px at 15% -10%, rgba(122,162,255,0.16), transparent 60%), radial-gradient(800px 700px at 95% 0%, rgba(180,120,255,0.14), transparent 55%), radial-gradient(700px 500px at 50% 110%, rgba(255,122,184,0.08), transparent 60%)",
      },
      keyframes: {
        twinkle: {
          "0%, 100%": { opacity: "0.85" },
          "50%": { opacity: "0.5" },
        },
        "fade-up": {
          "0%": { opacity: "0", transform: "translateY(8px)" },
          "100%": { opacity: "1", transform: "translateY(0)" },
        },
      },
      animation: {
        twinkle: "twinkle 6s ease-in-out infinite",
        "fade-up": "fade-up 0.4s ease-out both",
      },
    },
  },
  plugins: [],
} satisfies Config;
