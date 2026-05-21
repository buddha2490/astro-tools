import { NextResponse } from "next/server";
import { curateNight } from "@/lib/curation";

export const dynamic = "force-dynamic";

export async function POST(
  req: Request,
  { params }: { params: Promise<{ name: string }> },
) {
  const { name } = await params;
  const object = decodeURIComponent(name);
  try {
    const body = (await req.json()) as { date?: string; stems?: string[] };
    if (!body.date || !Array.isArray(body.stems)) {
      return NextResponse.json(
        { error: "Expected { date: string, stems: string[] }" },
        { status: 400 },
      );
    }
    const result = await curateNight(object, body.date, body.stems);
    return NextResponse.json(result);
  } catch (err) {
    console.error(`POST /api/object/${object}/curate failed`, err);
    return NextResponse.json({ error: String(err) }, { status: 500 });
  }
}
