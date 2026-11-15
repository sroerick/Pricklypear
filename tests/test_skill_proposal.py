import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from autolith.skill_proposal import MIN_ATTEMPTS, MIN_DISTINCT_TOOLS, propose_skill


def _propose(skills_dir, name="restart-the-shipper", tools=MIN_DISTINCT_TOOLS, attempts=MIN_ATTEMPTS):
    return propose_skill(
        name,
        distinct_tools=tools,
        attempts=attempts,
        trigger="the outbox shipper stalls after a schema migration",
        steps=["read the shipper log", "assert the cursor", "replay the batch"],
        verification="the outbox depth returns to zero",
        skills_dir=skills_dir,
    )


def test_above_threshold_one(tmp_path):
    skills = tmp_path / "skills"
    skills.mkdir()
    (skills / "unrelated.skill").write_text("trigger: something else\n")

    written = _propose(skills, tools=MIN_DISTINCT_TOOLS + 1)

    assert written is not None
    candidates = list((skills / "candidates").glob("*.skill"))
    assert len(candidates) == 1
    text = candidates[0].read_text()
    assert "trigger:" in text
    assert "steps:" in text
    assert "verification:" in text
    # the admitted skills directory is never a write target
    assert sorted(entry.name for entry in skills.iterdir() if entry.is_file()) == ["unrelated.skill"]


def test_below_threshold_none(tmp_path):
    skills = tmp_path / "skills"
    skills.mkdir()

    assert _propose(skills, tools=MIN_DISTINCT_TOOLS - 1) is None
    assert _propose(skills, attempts=MIN_ATTEMPTS - 1) is None
    assert not (skills / "candidates").exists()
    assert list(skills.iterdir()) == []


def test_rerun_no_duplicate(tmp_path):
    skills = tmp_path / "skills"
    skills.mkdir()

    first = _propose(skills)
    second = _propose(skills)

    assert first is not None
    assert second is None
    assert len(list((skills / "candidates").glob("*.skill"))) == 1

    # an admitted skill of that name also blocks a proposal
    (skills / "restart-the-shipper.skill").write_text("trigger: admitted\n")
    assert _propose(skills) is None
    assert len(list((skills / "candidates").glob("*.skill"))) == 1
