import { NextResponse } from "next/server";
import { getTonightSky } from "@/lib/queries";
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
    const data = await getTonightSky(slug, desig);
    if (!data) {
      return NextResponse.json(
        { error: "Coordinates not found for this object" },
        { status: 404 },
      );
    }
    return NextResponse.json(data);
  } catch (err) {
    console.error(`GET /api/catalog/${slug}/${desig}/tonight failed`, err);
    return NextResponse.json(
      { error: "Failed to compute tonight's sky" },
      { status: 500 },
    );
  }
}
