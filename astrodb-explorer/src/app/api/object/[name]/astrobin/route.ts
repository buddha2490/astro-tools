import { NextResponse } from "next/server";
import { getAstrobin } from "@/lib/queries";

export const dynamic = "force-dynamic";

export async function GET(
  _req: Request,
  { params }: { params: Promise<{ name: string }> },
) {
  const { name } = await params;
  const object = decodeURIComponent(name);
  try {
    const rows = await getAstrobin(object);
    return NextResponse.json(rows);
  } catch (err) {
    console.error(`GET astrobin for ${object} failed`, err);
    return NextResponse.json(
      { error: "Failed to load astrobin data" },
      { status: 500 },
    );
  }
}
