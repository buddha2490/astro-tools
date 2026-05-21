import { NextResponse } from "next/server";
import { getObjectNights } from "@/lib/curation";

export const dynamic = "force-dynamic";

export async function GET(
  _req: Request,
  { params }: { params: Promise<{ name: string }> },
) {
  const { name } = await params;
  const object = decodeURIComponent(name);
  try {
    return NextResponse.json({ nights: await getObjectNights(object) });
  } catch (err) {
    console.error(`GET /api/object/${object}/nights failed`, err);
    return NextResponse.json({ error: String(err) }, { status: 500 });
  }
}
