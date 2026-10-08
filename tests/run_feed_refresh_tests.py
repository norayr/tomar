"""Local-only refresh tests; use an isolated HOME and build directory."""

import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer


ROOT = Path(__file__).resolve().parents[1]
RSS = b"""<rss version="2.0"><channel><title>Test</title>
<item><title>Old item</title><guid>old</guid><description>Old content</description></item>
<item><title>New item</title><guid>new</guid><description>New content</description></item>
</channel></rss>"""
ATOM = b"""<feed xmlns="http://www.w3.org/2005/Atom"><entry>
<title>Atom item</title><id>atom</id><published>2026-10-01T12:00:00Z</published>
<updated>2026-10-08T12:00:00Z</updated><content>Atom content</content>
</entry></feed>"""
LARGE_RSS = ("<rss><channel>" + "".join(
    f"<item><guid>large{i}</guid><title>Item {i}</title><description>Content</description></item>"
    for i in range(4000)
) + "</channel></rss>").encode()


class Handler(BaseHTTPRequestHandler):
    counts = {}
    lock = threading.Lock()

    def do_GET(self):
        with self.lock:
            self.counts[self.path] = self.counts.get(self.path, 0) + 1
        if self.path in ("/slow", "/slow-last"):
            time.sleep(1.5)
        bodies = {"/slow": RSS, "/slow-last": RSS, "/atom": ATOM, "/large": LARGE_RSS, "/broken": b"not xml",
                  "/empty": b"<rss><channel/></rss>"}
        body = bodies.get(self.path, b"not found")
        self.send_response(200 if self.path in bodies else 404)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        try:
            self.wfile.write(body)
        except (BrokenPipeError, ConnectionResetError):
            pass  # Expected when testing cancellation.

    def log_message(self, *args):
        pass


def build(source, directory, extra=()):
    units = directory / source.stem
    units.mkdir()
    binary = directory / (source.stem + "-test")
    subprocess.run(["fpc", str(source), f"-Fu{ROOT}", f"-FU{units}",
                    f"-o{binary}", "-gl", *extra], check=True, cwd=ROOT)
    return binary


def main():
    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    base = f"http://127.0.0.1:{server.server_port}"
    try:
        temp_root = "/tmp/opencode" if Path("/tmp/opencode").is_dir() else None
        with tempfile.TemporaryDirectory(prefix="tomar-tests-", dir=temp_root) as tmp:
            directory = Path(tmp)
            worker = build(ROOT / "tests/FeedRefreshTests.lpr", directory, ["-gh"])
            subprocess.run([str(worker), base], check=True, timeout=20)

            if not os.environ.get("DISPLAY") and not shutil.which("xvfb-run"):
                print("UI tests skipped: set DISPLAY or install xvfb-run")
                return
            lazarus = Path(os.environ.get("LAZARUS", "/usr/share/lazarus"))
            cpu = subprocess.check_output(["fpc", "-iTP"], text=True).strip()
            system = subprocess.check_output(["fpc", "-iTO"], text=True).strip()
            arch = f"{cpu}-{system}"
            paths = [f"components/lazutils/lib/{arch}", f"lcl/units/{arch}",
                     f"lcl/units/{arch}/gtk2", f"packager/units/{arch}",
                     f"components/turbopower_ipro/units/{arch}/gtk2"]
            ui = build(ROOT / "tests/FeedRefreshUITests.lpr", directory,
                       [*(f"-Fu{lazarus / path}" for path in paths), "-dLCL", "-dLCLgtk2"])
            home = directory / "home"
            config = home / ".tomar"
            config.mkdir(parents=True)
            (config / "feeds.xml").write_text(f"""<feeds><item><name>Folder</name><type>folder</type>
<item><name>Slow</name><type>feed</type><url>{base}/slow</url></item>
<item><name>Broken</name><type>feed</type><url>{base}/broken</url></item>
<item><name>Atom</name><type>feed</type><url>{base}/atom</url></item></item>
<item><name>Outside</name><type>feed</type><url>{base}/outside</url></item>
<item><name>Large</name><type>feed</type><url>{base}/large</url></item></feeds>""")
            with Handler.lock:
                Handler.counts.clear()
            command = [str(ui), base, str(home)]
            if shutil.which("xvfb-run"):
                command = ["xvfb-run", "-a", *command]
            subprocess.run(command, check=True, timeout=60, env={**os.environ, "HOME": str(home)})
            with Handler.lock:
                assert Handler.counts.get("/slow") == 3, Handler.counts
                assert Handler.counts.get("/broken") == 1, Handler.counts
                assert Handler.counts.get("/atom") == 1, Handler.counts
                assert Handler.counts.get("/large") == 1, Handler.counts
                assert "/outside" not in Handler.counts, Handler.counts
    finally:
        server.shutdown()
        server.server_close()
        thread.join()


if __name__ == "__main__":
    main()
