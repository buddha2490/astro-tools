import { NextResponse } from "next/server";
import { removePlannedTarget } from "@/lib/queries";

export const dynamic = "force-dynamic";

export async function DELETE(
  _req: Request,
  { params }: { params: Promise<{ designation: string }> },
) {
  const { designation } = await params;
  const desig = decodeURIComponent(designation);
  try {
    await removePlannedTarget(desig);
    return NextResponse.json({ ok: true, planned: false });
  } catch (err) {
    console.error(`DELETE /api/planned/${desig} failed`, err);
    return NextResponse.json({ error: "Failed to remove planned target" }, { status: 500 });
  }
}
