import { getAstrobin, astrobinToCsv } from "@/lib/queries";

export const dynamic = "force-dynamic";

export async function GET(
  _req: Request,
  { params }: { params: Promise<{ name: string }> },
) {
  const { name } = await params;
  const object = decodeURIComponent(name);
  try {
    const rows = await getAstrobin(object);
    const csv = astrobinToCsv(rows);
    return new Response(csv, {
      status: 200,
      headers: {
        "Content-Type": "text/csv; charset=utf-8",
        "Content-Disposition": `attachment; filename="${object}_astrobin_subs.csv"`,
      },
    });
  } catch (err) {
    console.error(`GET astrobin.csv for ${object} failed`, err);
    return new Response("Failed to build CSV", { status: 500 });
  }
}
