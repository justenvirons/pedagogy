"""Generate the figures used in week03-geller-typology.qmd.

Run:  python3 make_figures.py      (writes PNGs into ./images)
Requires matplotlib only.
"""
from pathlib import Path
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, Rectangle, Circle, Polygon, FancyArrowPatch

OUT = Path(__file__).parent / "images"
OUT.mkdir(exist_ok=True)

INK = "#0b0b0b"
INK2 = "#52514e"
SURFACE = "#fcfcfb"
# categorical slots 1-4 (validated adjacent order)
C_SF, C_EC, C_IC, C_NW = "#2a78d6", "#eb6834", "#1baf7a", "#eda100"
TYPE_COLORS = {"Strong & Fearless": C_SF, "Enthused & Confident": C_EC,
               "Interested but Concerned": C_IC, "No Way No How": C_NW}

plt.rcParams.update({"font.family": "DejaVu Sans", "text.color": INK,
                     "axes.edgecolor": INK2, "savefig.facecolor": SURFACE})

# ---------------------------------------------------------------- street kit
ASPHALT = "#4a4a48"
SIDEWALK = "#d9d6cf"
GRASS = "#9cc58a"
GREEN_PAINT = "#3f9d57"
PATH = "#c9b89a"
CURB = "#8f8c85"
CAR_COLORS = ["#c0392b", "#2e5d8a", "#e0e0dc", "#6b6b6b", "#b8860b", "#1f3a52"]


def car(ax, x, y, direction=1, color="#2e5d8a", moving=True, L=9.0, W=3.6):
    body = FancyBboxPatch((x - L / 2, y - W / 2), L, W,
                          boxstyle="round,pad=0,rounding_size=1.1",
                          fc=color, ec="#1a1a1a", lw=1.0, zorder=5)
    ax.add_patch(body)
    # windshield toward the direction of travel
    wx = x + direction * L * 0.18
    ax.add_patch(Rectangle((wx - 1.1, y - W / 2 + 0.45), 2.2, W - 0.9,
                           fc="#9fb7c9", ec="none", zorder=6))
    ax.add_patch(Rectangle((x - direction * L * 0.33 - 0.6, y - W / 2 + 0.6), 1.2, W - 1.2,
                           fc="#9fb7c9", ec="none", zorder=6))
    if moving:  # speed lines behind the car
        for dy in (-0.9, 0, 0.9):
            x0 = x - direction * (L / 2 + 0.6)
            ax.plot([x0, x0 - direction * 3.2], [y + dy, y + dy],
                    color="#f2f2f2", lw=1.3, alpha=0.8, zorder=4)


def cyclist(ax, x, y, direction=1, label=True):
    ax.plot([x - 2.1, x + 2.1], [y, y], color=INK, lw=3.2, solid_capstyle="round", zorder=8)
    ax.add_patch(Circle((x - direction * 0.3, y), 0.95, fc="#ffcc33", ec=INK, lw=1.2, zorder=9))
    if label:
        ax.annotate("You", xy=(x, y + 1.2), xytext=(x, y + 5.2), ha="center",
                    fontsize=15, fontweight="bold", color=INK, zorder=10,
                    bbox=dict(boxstyle="round,pad=0.25", fc="#ffcc33", ec=INK, lw=1),
                    arrowprops=dict(arrowstyle="-|>", color=INK, lw=1.5))


def strip(ax, y0, h, color, x0=0, x1=100, z=1):
    ax.add_patch(Rectangle((x0, y0), x1 - x0, h, fc=color, ec="none", zorder=z))


def dashed(ax, y, color="#f5f5f0", lw=2.0, dash=(4, 4)):
    ax.plot([0, 100], [y, y], color=color, lw=lw, dashes=dash, zorder=3)


def solid(ax, y, color="#f5f5f0", lw=2.0):
    ax.plot([0, 100], [y, y], color=color, lw=lw, zorder=3)


def tree(ax, x, y, r=3.0):
    ax.add_patch(Circle((x, y), r, fc="#4f8a3c", ec="#2f5a22", lw=1, zorder=7, alpha=0.95))


def badge(ax, lines):
    ax.text(1.5, ax.get_ylim()[1] - 1.2, "\n".join(lines), va="top", ha="left",
            fontsize=13, color=INK, zorder=20, linespacing=1.4,
            bbox=dict(boxstyle="round,pad=0.5", fc="white", ec=INK2, lw=1, alpha=0.95))


def new_street(height):
    fig, ax = plt.subplots(figsize=(12, 12 * height / 100))
    ax.set_xlim(0, 100)
    ax.set_ylim(0, height)
    ax.set_aspect("equal")
    ax.axis("off")
    fig.subplots_adjust(0, 0, 1, 1)
    return fig, ax


def save(fig, name):
    fig.savefig(OUT / name, dpi=150)
    plt.close(fig)


# S1 – major street, no bike lane ---------------------------------------------
def s1():
    fig, ax = new_street(46)
    y = 0
    strip(ax, y, 4, SIDEWALK); y += 4
    strip(ax, y, 38, ASPHALT); base = y
    y += 4  # sidewalk top
    ax.plot([0, 100], [4, 4], color=CURB, lw=3, zorder=3)
    ax.plot([0, 100], [42, 42], color=CURB, lw=3, zorder=3)
    strip(ax, 42, 4, SIDEWALK)
    # parking 3.5 | lane 7.5 | lane 7.5 || lane 7.5 | lane 7.5 | parking 3.5  (=38 -> scale)
    # lanes (y from 4): parking 4-8, EB lanes 8-15.5,15.5-23, center 23, WB 23-30.5,30.5-38, parking 38-42
    solid(ax, 8, "#bdbdb5", 1.2); solid(ax, 38, "#bdbdb5", 1.2)
    dashed(ax, 15.5)
    ax.plot([0, 100], [22.7, 22.7], color="#f2c200", lw=2, zorder=3)
    ax.plot([0, 100], [23.3, 23.3], color="#f2c200", lw=2, zorder=3)
    dashed(ax, 30.5)
    for i, x in enumerate([8, 20, 52, 64, 88]):
        car(ax, x, 6, color=CAR_COLORS[i % 6], moving=False, W=3.2)
    for i, x in enumerate([12, 70]):
        car(ax, x, 40, color=CAR_COLORS[(i + 2) % 6], moving=False, W=3.2)
    car(ax, 30, 11.6, 1, CAR_COLORS[1]); car(ax, 70, 11.6, 1, CAR_COLORS[3])
    car(ax, 48, 19.2, 1, CAR_COLORS[0]); car(ax, 90, 19.2, 1, CAR_COLORS[5])
    car(ax, 20, 26.8, -1, CAR_COLORS[4]); car(ax, 62, 26.8, -1, CAR_COLORS[2])
    car(ax, 38, 34.2, -1, CAR_COLORS[1]); car(ax, 84, 34.2, -1, CAR_COLORS[0])
    cyclist(ax, 55, 9.6)
    ax.set_ylim(0, 46)
    badge(ax, ["Major street · NO bike lane", "2 lanes each way · 30–35 mph · heavy traffic"])
    save(fig, "s1_major_no_lane.png")


# S2 – major street, painted bike lane ------------------------------------------
def s2():
    fig, ax = new_street(46)
    strip(ax, 0, 4, SIDEWALK); strip(ax, 4, 38, ASPHALT); strip(ax, 42, 4, SIDEWALK)
    ax.plot([0, 100], [4, 4], color=CURB, lw=3, zorder=3)
    ax.plot([0, 100], [42, 42], color=CURB, lw=3, zorder=3)
    # parking 4-7.5, bike lane 7.5-10.5, lanes 10.5-16.5, 16.5-23 | 23-29.5, 29.5-35.5, bike 35.5-38.5, parking 38.5-42
    strip(ax, 7.6, 3, GREEN_PAINT, z=2); strip(ax, 35.4, 3, GREEN_PAINT, z=2)
    solid(ax, 7.5); solid(ax, 10.6); solid(ax, 35.4); solid(ax, 38.5)
    dashed(ax, 16.7)
    ax.plot([0, 100], [22.7, 22.7], color="#f2c200", lw=2, zorder=3)
    ax.plot([0, 100], [23.3, 23.3], color="#f2c200", lw=2, zorder=3)
    dashed(ax, 29.3)
    for i, x in enumerate([8, 22, 50, 66, 90]):
        car(ax, x, 5.75, color=CAR_COLORS[i % 6], moving=False, W=3.0)
    for i, x in enumerate([14, 58, 80]):
        car(ax, x, 40.25, color=CAR_COLORS[(i + 3) % 6], moving=False, W=3.0)
    car(ax, 30, 13.6, 1, CAR_COLORS[1]); car(ax, 78, 13.6, 1, CAR_COLORS[3])
    car(ax, 52, 19.8, 1, CAR_COLORS[0])
    car(ax, 22, 26.2, -1, CAR_COLORS[4]); car(ax, 66, 26.2, -1, CAR_COLORS[2])
    car(ax, 42, 32.4, -1, CAR_COLORS[5])
    for bx in (18, 82):  # bike stencils
        ax.text(bx, 9.05, "⟵ BIKE ⟶" if False else "BIKE", ha="center", va="center",
                color="white", fontsize=8, fontweight="bold", zorder=4)
    cyclist(ax, 58, 9.05)
    badge(ax, ["Major street · PAINTED bike lane", "2 lanes each way · 30–35 mph · heavy traffic"])
    save(fig, "s2_major_painted_lane.png")


# S3 – major street, protected bike lane ----------------------------------------
def s3():
    fig, ax = new_street(46)
    strip(ax, 0, 4, SIDEWALK); strip(ax, 4, 38, ASPHALT); strip(ax, 42, 4, SIDEWALK)
    ax.plot([0, 100], [4, 4], color=CURB, lw=3, zorder=3)
    ax.plot([0, 100], [42, 42], color=CURB, lw=3, zorder=3)
    # bike 4-7.5 | buffer w/ posts 7.5-9 | parking 9-12.5 | lanes 12.5-18, 18-23 | 23-28, 28-33.5 | parking 33.5-37 | buffer 37-38.5 | bike 38.5-42
    strip(ax, 4.1, 3.4, GREEN_PAINT, z=2); strip(ax, 38.5, 3.4, GREEN_PAINT, z=2)
    for yb in (7.5, 37.0):
        strip(ax, yb, 1.5, "#e8e6df", z=2)
        for px in range(2, 100, 4):
            ax.add_patch(Circle((px, yb + 0.75), 0.45, fc="white", ec="#333", lw=0.8, zorder=4))
    solid(ax, 12.5, "#bdbdb5", 1.2); solid(ax, 33.5, "#bdbdb5", 1.2)
    dashed(ax, 18)
    ax.plot([0, 100], [22.7, 22.7], color="#f2c200", lw=2, zorder=3)
    ax.plot([0, 100], [23.3, 23.3], color="#f2c200", lw=2, zorder=3)
    dashed(ax, 28)
    for i, x in enumerate([10, 24, 54, 70, 88]):
        car(ax, x, 10.75, color=CAR_COLORS[i % 6], moving=False, W=3.0)
    for i, x in enumerate([16, 60, 84]):
        car(ax, x, 35.25, color=CAR_COLORS[(i + 3) % 6], moving=False, W=3.0)
    car(ax, 36, 15.3, 1, CAR_COLORS[1], W=3.3); car(ax, 80, 20.4, 1, CAR_COLORS[3], W=3.3)
    car(ax, 20, 25.6, -1, CAR_COLORS[4], W=3.3); car(ax, 64, 30.7, -1, CAR_COLORS[0], W=3.3)
    cyclist(ax, 46, 5.8, label=False)
    ax.annotate("You", xy=(46, 5.8), xytext=(46, 1.0), ha="center", va="bottom", fontsize=13,
                fontweight="bold", bbox=dict(boxstyle="round,pad=0.2", fc="#ffcc33", ec=INK),
                zorder=12)
    badge(ax, ["Major street · PROTECTED bike lane", "Posts + parked cars separate you from traffic"])
    save(fig, "s3_major_protected_lane.png")


# S4 – quiet residential street / neighborhood greenway --------------------------
def s4():
    fig, ax = new_street(46)
    strip(ax, 0, 5, GRASS); strip(ax, 5, 4, SIDEWALK)
    strip(ax, 9, 28, ASPHALT)
    strip(ax, 37, 4, SIDEWALK); strip(ax, 41, 5, GRASS)
    ax.plot([0, 100], [9, 9], color=CURB, lw=3, zorder=3)
    ax.plot([0, 100], [37, 37], color=CURB, lw=3, zorder=3)
    for x in range(6, 100, 16):
        tree(ax, x, 2.6, 2.4); tree(ax, x + 8, 43.4, 2.4)
    # parking 9-12.5 | travel 12.5-33.5 | parking 33.5-37  — no center line
    for i, x in enumerate([14, 44, 80]):
        car(ax, x, 10.75, color=CAR_COLORS[i % 6], moving=False, W=3.0)
    for i, x in enumerate([30, 68]):
        car(ax, x, 35.25, color=CAR_COLORS[(i + 2) % 6], moving=False, W=3.0)
    # speed hump + sharrow
    ax.add_patch(Rectangle((60, 12.5), 2.2, 21, fc="#e2c044", ec="none", zorder=3, alpha=0.9))
    for sx in (25, 85):
        ax.add_patch(Polygon([[sx, 18], [sx + 2.2, 19.3], [sx, 20.6]], fc="white", zorder=3))
        ax.add_patch(Polygon([[sx - 2.4, 18], [sx - 0.2, 19.3], [sx - 2.4, 20.6]], fc="white", zorder=3))
    car(ax, 84, 28.2, -1, CAR_COLORS[2], moving=False)
    cyclist(ax, 44, 17)
    badge(ax, ["Quiet residential street / greenway", "1 lane each way · 20 mph · few cars · speed humps"])
    save(fig, "s4_residential_greenway.png")


# S5 – off-street path -----------------------------------------------------------
def s5():
    fig, ax = new_street(46)
    strip(ax, 0, 46, GRASS)
    strip(ax, 18, 10, PATH)
    ax.plot([0, 100], [23, 23], color="white", lw=1.5, dashes=(3, 4), zorder=3)
    for x in range(4, 100, 11):
        tree(ax, x, 8 + (x % 3), 3.2); tree(ax, x + 5, 37 - (x % 4), 3.2)
    ax.add_patch(Rectangle((0, 44.5), 100, 1.5, fc="#6fa8dc", ec="none", zorder=2))  # lake edge
    cyclist(ax, 40, 20.5)
    cyclist(ax, 72, 25.5, direction=-1, label=False)
    ax.add_patch(Circle((20, 26), 0.9, fc="#8e7cc3", ec=INK, zorder=9))  # walker
    badge(ax, ["Off-street path or trail", "No cars at all · shared with walkers & runners"])
    save(fig, "s5_offstreet_path.png")


# ------------------------------------------------------------ typology bar chart
def typology_bar():
    fig, ax = plt.subplots(figsize=(12, 3.6))
    fig.subplots_adjust(0.02, 0.28, 0.98, 0.82)
    shares = [("Strong & Fearless", 0.5), ("Enthused & Confident", 7),
              ("Interested but Concerned", 60), ("No Way No How", 33)]
    left = 0
    for name, v in shares:
        w = max(v, 0.9)
        ax.barh(0, w - 0.35, left=left, color=TYPE_COLORS[name], height=0.7, edgecolor="none")
        left += w
    ax.text(42, 0, "Interested but Concerned\n60%", ha="center", va="center",
            fontsize=15, color="white", fontweight="bold")
    ax.text(83.5, 0, "No Way No How\n33%", ha="center", va="center", fontsize=15,
            color=INK, fontweight="bold")
    ax.annotate("Strong & Fearless  <1%", xy=(0.45, -0.36), xytext=(1, -0.85),
                fontsize=12.5, ha="left", va="top", color=INK,
                arrowprops=dict(arrowstyle="-", color=INK2))
    ax.annotate("Enthused & Confident  7%", xy=(4.8, 0.36), xytext=(1, 0.85),
                fontsize=12.5, ha="left", va="bottom", color=INK,
                arrowprops=dict(arrowstyle="-", color=INK2))
    ax.set_xlim(0, 101); ax.set_ylim(-1.3, 1.3)
    ax.axis("off")
    fig.text(0.02, 0.93, "Geller's estimates for Portland", fontsize=16,
             fontweight="bold", color=INK)
    fig.text(0.02, 0.06, "Share of city population by relationship to bicycling for transportation. "
             "Estimates from professional judgment, later compared with survey data.",
             fontsize=10.5, color=INK2)
    save(fig, "four_types_bar.png")


# ------------------------------------------------------------ scoring flowchart
def scoring_tree():
    fig, ax = plt.subplots(figsize=(12, 6.4))
    ax.set_xlim(0, 120); ax.set_ylim(-2.5, 68); ax.axis("off")
    fig.subplots_adjust(0, 0, 1, 1)

    def q(x, y, text, w=34, h=8.5):
        ax.add_patch(FancyBboxPatch((x - w / 2, y - h / 2), w, h,
                                    boxstyle="round,pad=0.3,rounding_size=1.5",
                                    fc="white", ec=INK2, lw=1.4, zorder=3))
        ax.text(x, y, text, ha="center", va="center", fontsize=11.5, zorder=4, linespacing=1.3)

    def out(x, y, text, color, w=26, h=7, dark_text=False):
        ax.add_patch(FancyBboxPatch((x - w / 2, y - h / 2), w, h,
                                    boxstyle="round,pad=0.3,rounding_size=1.5",
                                    fc=color, ec="none", zorder=3))
        ax.text(x, y, text, ha="center", va="center", fontsize=12.5, fontweight="bold",
                color=INK if dark_text else "white", zorder=4)

    def arrow(p0, p1, label=None, lx=0, ly=0):
        ax.add_patch(FancyArrowPatch(p0, p1, arrowstyle="-|>", mutation_scale=14,
                                     color=INK2, lw=1.4, zorder=2))
        if label:
            ax.text((p0[0] + p1[0]) / 2 + lx, (p0[1] + p1[1]) / 2 + ly, label,
                    fontsize=11, fontweight="bold", color=INK2, ha="center", va="center",
                    bbox=dict(fc=SURFACE, ec="none", pad=1), zorder=5)

    X = 30
    q(X, 57, "Step 1 · Able to ride a bike?\n(A1)")
    q(X, 42, "Step 2 · Interested OR riding now?\n(A2 agree  or  A3 = yes)")
    q(X, 27, "Step 3 · VERY comfortable on\nmajor street, NO bike lane? (S1 = 4)")
    q(X, 12, "Step 4 · VERY comfortable on\nmajor street, PAINTED lane? (S2 = 4)")
    out(95, 49.5, "No Way No How", C_NW, dark_text=True)
    out(95, 27, "Strong & Fearless", C_SF)
    out(95, 12, "Enthused & Confident", C_EC)
    out(64, 3.4, "Interested but Concerned", C_IC, w=34)
    arrow((X, 52.5), (X, 46.5), "yes", lx=3)
    arrow((X, 37.5), (X, 31.5), "yes", lx=3)
    arrow((X, 22.5), (X, 16.5), "no", lx=3)
    arrow((X + 17.3, 57), (82, 51.5), "no", ly=2.2)
    arrow((X + 17.3, 42), (82, 47.5), "no", ly=-2.2)
    arrow((X + 17.3, 27), (81.7, 27), "yes", ly=2)
    arrow((X + 17.3, 12), (81.7, 12), "yes", ly=2)
    arrow((X, 7.5), (46.7, 3.6), "no", lx=-2, ly=-1.2)
    ax.text(2, 67, "Also: S5 = 1 (very uncomfortable even on a car-free path) → No Way No How",
            fontsize=10, color=INK2, va="top")
    save(fig, "scoring_tree.png")


# --------------------------------------------------------------- comfort ladder
def comfort_ladder():
    """Illustrative comfort profiles by type (stylized, for teaching)."""
    fig, ax = plt.subplots(figsize=(12, 5.2))
    fig.subplots_adjust(0.1, 0.27, 0.76, 0.88)
    xs = ["S1\nNo lane", "S2\nPainted", "S3\nProtected", "S4\nGreenway", "S5\nPath"]
    prof = {"Strong & Fearless": [4, 4, 4, 4, 4],
            "Enthused & Confident": [2.2, 4, 4, 4, 4],
            "Interested but Concerned": [1.2, 2.2, 3.4, 3.6, 3.9],
            "No Way No How": [1, 1.2, 1.6, 1.9, 2.2]}
    offs = {"Strong & Fearless": 0.06, "Enthused & Confident": -0.03,
            "Interested but Concerned": 0, "No Way No How": 0}
    for name, ys in prof.items():
        ys2 = [v + offs[name] for v in ys]
        ax.plot(range(5), ys2, color=TYPE_COLORS[name], lw=2.4, marker="o", ms=8,
                markeredgecolor=SURFACE, markeredgewidth=2, label=name)
        ly = {"Strong & Fearless": 4.28, "Enthused & Confident": 4.0,
              "Interested but Concerned": 3.72, "No Way No How": 2.2}[name]
        ax.text(4.12, ly, name, color=INK, fontsize=12, va="center")
    ax.set_xticks(range(5)); ax.set_xticklabels(xs, fontsize=11, color=INK2)
    ax.set_yticks([1, 2, 3, 4])
    ax.set_yticklabels(["1 Very\nuncomfortable", "2", "3", "4 Very\ncomfortable"], fontsize=10, color=INK2)
    ax.set_ylim(0.7, 4.35); ax.set_xlim(-0.2, 4.1)
    ax.grid(axis="y", color="#e4e2dc", lw=0.8); ax.set_axisbelow(True)
    for s in ("top", "right", "left"):
        ax.spines[s].set_visible(False)
    ax.tick_params(length=0)
    ax.legend(loc="upper left", bbox_to_anchor=(-0.02, -0.2), ncol=4, frameon=False, fontsize=10.5)
    fig.text(0.1, 0.93, "What the types look like as comfort profiles (stylized)", fontsize=15,
             fontweight="bold")
    fig.text(0.1, 0.03, "Illustrative only — the gap between S1 and S3–S5 is each person's 'comfort gain' from separation.",
             fontsize=10.5, color=INK2)
    save(fig, "comfort_profiles.png")


if __name__ == "__main__":
    s1(); s2(); s3(); s4(); s5()
    typology_bar(); scoring_tree(); comfort_ladder()
    print("figures written to", OUT)
