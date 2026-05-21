import { NextResponse } from "next/server";
import { resolveCoord } from "@/lib/astro/coordinates";
import { getDssPath, fetchAndStoreDss } from "@/lib/dss";

export const dynamic = "force-dynamic";

export async function GET(
  _req: Request,
  { params }: { params: Promise<{ name: string }> },
) {
  const { name } = await params;
  const object = decodeURIComponent(name);
  try {
    return NextResponse.json({ path: await getDssPath(object) });
  } catch (err) {
    console.error(`GET /api/object/${object}/dss failed`, err);
    return NextResponse.json({ error: "Failed" }, { status: 500 });
  }
}

export async function POST(
  _req: Request,
  { params }: { params: Promise<{ name: string }> },
) {
  const { name } = await params;
  const object = decodeURIComponent(name);
  try {
    const coord = await resolveCoord(object);
    if (!coord) {
      return NextResponse.json(
        { error: `Could not resolve coordinates for "${object}"` },
        { status: 422 },
      );
    }
    const path = await fetchAndStoreDss(object, coord.raDeg, coord.decDeg);
    return NextResponse.json({ path });
  } catch (err) {
    console.error(`POST /api/object/${object}/dss failed`, err);
    return NextResponse.json({ error: String(err) }, { status: 500 });
  }
}
