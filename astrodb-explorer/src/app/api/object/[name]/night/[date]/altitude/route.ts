import { NextResponse } from "next/server";
import { getAltitudeNight } from "@/lib/queries";

export const dynamic = "force-dynamic";

export async function GET(
  _req: Request,
  { params }: { params: Promise<{ name: string; date: string }> },
) {
  const { name, date } = await params;
  const object = decodeURIComponent(name);
  const night = decodeURIComponent(date);
  try {
    const data = await getAltitudeNight(object, night);
    if (!data) {
      return NextResponse.json(
        { error: "Coordinates not found for this target" },
        { status: 404 },
      );
    }
    return NextResponse.json(data);
  } catch (err) {
    console.error(`GET /api/object/${object}/night/${night}/altitude failed`, err);
    return NextResponse.json(
      { error: "Failed to compute altitude curve" },
      { status: 500 },
    );
  }
}
