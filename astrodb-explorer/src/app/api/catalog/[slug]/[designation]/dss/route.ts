import { NextResponse } from "next/server";
import { isCatalogSlug } from "@/lib/catalog";
import { getCatalogCoord } from "@/lib/queries";
import { getDssPath, fetchAndStoreDss } from "@/lib/dss";

export const dynamic = "force-dynamic";

// DSS images are keyed by name in the shared dss_images table, so a catalog
// designation that matches a captured object (e.g. "M81") shares the same
// cached image — and a fetch here also populates that object's thumbnail.

export async function GET(
  _req: Request,
  { params }: { params: Promise<{ slug: string; designation: string }> },
) {
  const { designation } = await params;
  const desig = decodeURIComponent(designation);
  try {
    return NextResponse.json({ path: await getDssPath(desig) });
  } catch (err) {
    console.error(`GET /api/catalog/.../${desig}/dss failed`, err);
    return NextResponse.json({ error: "Failed" }, { status: 500 });
  }
}

export async function POST(
  _req: Request,
  { params }: { params: Promise<{ slug: string; designation: string }> },
) {
  const { slug, designation } = await params;
  if (!isCatalogSlug(slug)) {
    return NextResponse.json({ error: "Unknown catalog" }, { status: 404 });
  }
  const desig = decodeURIComponent(designation);
  try {
    const coord = await getCatalogCoord(slug, desig);
    if (!coord) {
      return NextResponse.json(
        { error: `No coordinates on file for "${desig}"` },
        { status: 422 },
      );
    }
    const path = await fetchAndStoreDss(desig, coord.raDeg, coord.decDeg);
    return NextResponse.json({ path });
  } catch (err) {
    console.error(`POST /api/catalog/${slug}/${desig}/dss failed`, err);
    return NextResponse.json({ error: String(err) }, { status: 500 });
  }
}
