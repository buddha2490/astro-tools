import { NextResponse } from "next/server";
import { getCatalog } from "@/lib/queries";
import { isCatalogSlug } from "@/lib/catalog";

export const dynamic = "force-dynamic";

export async function GET(
  _req: Request,
  { params }: { params: Promise<{ slug: string }> },
) {
  const { slug } = await params;
  if (!isCatalogSlug(slug)) {
    return NextResponse.json({ error: "Unknown catalog" }, { status: 404 });
  }
  try {
    const data = await getCatalog(slug);
    return NextResponse.json(data);
  } catch (err) {
    console.error(`GET /api/catalog/${slug} failed`, err);
    return NextResponse.json({ error: "Failed to load catalog" }, { status: 500 });
  }
}
