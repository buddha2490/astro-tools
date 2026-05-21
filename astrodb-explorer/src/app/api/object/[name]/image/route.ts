import { NextResponse } from "next/server";
import { getObjectImagePath, storeObjectImage } from "@/lib/images";

export const dynamic = "force-dynamic";
// Allow long uploads of large (~50 MB) images.
export const maxDuration = 300;

export async function GET(
  _req: Request,
  { params }: { params: Promise<{ name: string }> },
) {
  const { name } = await params;
  const object = decodeURIComponent(name);
  try {
    return NextResponse.json({ path: await getObjectImagePath(object) });
  } catch (err) {
    console.error(`GET /api/object/${object}/image failed`, err);
    return NextResponse.json({ error: "Failed" }, { status: 500 });
  }
}

export async function POST(
  req: Request,
  { params }: { params: Promise<{ name: string }> },
) {
  const { name } = await params;
  const object = decodeURIComponent(name);
  try {
    const form = await req.formData();
    const file = form.get("file");
    if (!(file instanceof File)) {
      return NextResponse.json(
        { error: "No file provided (expected multipart field 'file')" },
        { status: 400 },
      );
    }
    const buf = Buffer.from(await file.arrayBuffer());
    const path = await storeObjectImage(object, buf, file.name || "image");
    return NextResponse.json({ path });
  } catch (err) {
    console.error(`POST /api/object/${object}/image failed`, err);
    return NextResponse.json({ error: String(err) }, { status: 500 });
  }
}
