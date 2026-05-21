import { NextResponse } from "next/server";
import { getPlannedTargets, addPlannedTarget } from "@/lib/queries";
import { isCatalogSlug } from "@/lib/catalog";

export const dynamic = "force-dynamic";

export async function GET() {
  try {
    return NextResponse.json(await getPlannedTargets());
  } catch (err) {
    console.error("GET /api/planned failed", err);
    return NextResponse.json({ error: "Failed to load planned targets" }, { status: 500 });
  }
}

export async function POST(req: Request) {
  let body: { slug?: string; designation?: string };
  try {
    body = await req.json();
  } catch {
    return NextResponse.json({ error: "Invalid JSON body" }, { status: 400 });
  }
  const { slug, designation } = body;
  if (!slug || !isCatalogSlug(slug) || !designation) {
    return NextResponse.json({ error: "slug and designation are required" }, { status: 400 });
  }
  try {
    const ok = await addPlannedTarget(slug, designation);
    if (!ok) {
      return NextResponse.json({ error: "Object not found in catalog" }, { status: 404 });
    }
    return NextResponse.json({ ok: true, planned: true });
  } catch (err) {
    console.error(`POST /api/planned (${slug}/${designation}) failed`, err);
    return NextResponse.json({ error: "Failed to add planned target" }, { status: 500 });
  }
}
