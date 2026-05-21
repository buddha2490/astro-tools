import { redirect } from "next/navigation";

// /catalog has no view of its own — land on the first catalog.
export default function CatalogIndex() {
  redirect("/catalog/messier");
}
