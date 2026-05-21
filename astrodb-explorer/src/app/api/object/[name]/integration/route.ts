import { NextResponse } from "next/server";
import { getIntegration } from "@/lib/queries";

export const dynamic = "force-dynamic";

export async function GET(
  _req: Request,
  { params }: { params: Promise<{ name: string }> },
) {
  const { name } = await params;
  const object = decodeURIComponent(name);
  try {
    const rows = await getIntegration(object);
    return NextResponse.json(rows);
  } catch (err) {
    console.error(`GET integration for ${object} failed`, err);
    return NextResponse.json(
      { error: "Failed to load integration" },
      { status: 500 },
    );
  }
}
