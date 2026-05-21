import { NextResponse } from "next/server";
import { bulkInsertFrames } from "@/lib/bulk";
import type { AstroSubRecord } from "@/lib/nina/ingest/mapper";

export const dynamic = "force-dynamic";

// POST { records: AstroSubRecord[] } — header-parsed light frames from the
// client's chosen folder. Inserts Final='Kept', dedups by stem.
export async function POST(req: Request) {
  try {
    const body = (await req.json()) as { records?: AstroSubRecord[] };
    if (!Array.isArray(body.records)) {
      return NextResponse.json(
        { error: "Expected { records: AstroSubRecord[] }" },
        { status: 400 },
      );
    }
    if (body.records.length === 0) {
      return NextResponse.json({ inserted: 0, duplicates: 0, total: 0 });
    }
    return NextResponse.json(await bulkInsertFrames(body.records));
  } catch (err) {
    console.error("POST /api/bulk-add failed", err);
    return NextResponse.json({ error: String(err) }, { status: 500 });
  }
}
