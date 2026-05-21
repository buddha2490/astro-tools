import { NextResponse } from "next/server";
import { getCatalogObject } from "@/lib/queries";
import { isCatalogSlug } from "@/lib/catalog";

export const dynamic = "force-dynamic";

export async function GET(
  _req: Request,
  { params }: { params: Promise<{ slug: string; designation: string }> },
) {
  const { slug, designation } = await params;
  if (!isCatalogSlug(slug)) {
    return NextResponse.json({ error: "Unknown catalog" }, { status: 404 });
  }
  const desig = decodeURIComponent(designation);
  try {
    const data = await getCatalogObject(slug, desig);
    if (!data) {
      return NextResponse.json({ error: "Object not found" }, { status: 404 });
    }
    return NextResponse.json(data);
  } catch (err) {
    console.error(`GET /api/catalog/${slug}/${desig} failed`, err);
    return NextResponse.json({ error: "Failed to load object" }, { status: 500 });
  }
}
