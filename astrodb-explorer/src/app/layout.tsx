import type { Metadata } from "next";
import "./globals.css";
import Nav from "./components/Nav";

export const metadata: Metadata = {
  title: "astroDB Explorer",
  description: "Explore the astrophotography database",
};

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <html lang="en" className="dark">
      <body className="bg-space text-fg font-sans antialiased">
        {/* fixed cosmic backdrop behind all content */}
        <div className="space-bg" aria-hidden />
        <div className="starfield" aria-hidden />
        <Nav />
        {children}
      </body>
    </html>
  );
}
