import { NextResponse } from "next/server";
import { getObjectDetail } from "@/lib/queries";

export const dynamic = "force-dynamic";

export async function GET(
  _req: Request,
  { params }: { params: Promise<{ name: string }> },
) {
  const { name } = await params;
  const object = decodeURIComponent(name);
  try {
    const detail = await getObjectDetail(object);
    if (!detail) {
      return NextResponse.json({ error: "Not found" }, { status: 404 });
    }
    return NextResponse.json(detail);
  } catch (err) {
    console.error(`GET /api/object/${object}/detail failed`, err);
    return NextResponse.json(
      { error: "Failed to load object detail" },
      { status: 500 },
    );
  }
}
