import { NextResponse } from "next/server";
import { getSummary } from "@/lib/queries";

export const dynamic = "force-dynamic";

export async function GET() {
  try {
    const rows = await getSummary();
    return NextResponse.json(rows);
  } catch (err) {
    console.error("GET /api/summary failed", err);
    return NextResponse.json(
      { error: "Failed to load summary" },
      { status: 500 },
    );
  }
}
