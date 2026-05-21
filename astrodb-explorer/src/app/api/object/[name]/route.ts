import { NextResponse } from "next/server";
import { deleteObject } from "@/lib/queries";
import { deleteObjectImage } from "@/lib/images";

export const dynamic = "force-dynamic";

// DELETE /api/object/:name — permanently remove all of an object's frames and
// its uploaded image. Destructive; the object can be re-added later.
export async function DELETE(
  _req: Request,
  { params }: { params: Promise<{ name: string }> },
) {
  const { name } = await params;
  const object = decodeURIComponent(name);
  try {
    const { deletedFrames } = await deleteObject(object);
    await deleteObjectImage(object).catch((e) =>
      console.error(`deleteObjectImage(${object}) failed`, e),
    );
    return NextResponse.json({ ok: true, deletedFrames });
  } catch (err) {
    console.error(`DELETE /api/object/${object} failed`, err);
    return NextResponse.json({ error: String(err) }, { status: 500 });
  }
}
