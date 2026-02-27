"""
run_queries.py
--------------
Demonstration script for the ClinicalTrialDataAgent.

Runs 3 example natural-language queries against the ADAE dataset and prints
structured results to stdout.

Usage
-----
    python run_queries.py

Set the ANTHROPIC_API_KEY environment variable to use the live LLM.
Without it the script falls back to the keyword-based mock automatically.
"""

from __future__ import annotations

import os
from pathlib import Path

from clinical_data_agent import ClinicalTrialDataAgent

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
DATA_PATH = Path(__file__).parent / "data" / "adae.csv"

QUERIES = [
    "Give me the subjects who had adverse events of Moderate severity",
    "Which patients experienced Dizziness?",
    "Show me subjects with cardiac system adverse events",
]


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _truncate_ids(ids: list[str], limit: int = 10) -> str:
    """Return a compact string representation of subject IDs."""
    if not ids:
        return "[]"
    shown = ids[:limit]
    suffix = f" ... (+{len(ids) - limit} more)" if len(ids) > limit else ""
    return "[" + ", ".join(shown) + "]" + suffix


def print_result(result: dict) -> None:
    """Pretty-print a single query result."""
    print(f"\nQuery   : {result['question']}")
    print(f"→ Parsed: {result['parsed']}")
    print(f"→ Results: {result['count']} unique subjects")
    print(f"→ Subject IDs: {_truncate_ids(result['subject_ids'])}")
    if "error" in result:
        print(f"  [!] {result['error']}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    api_key = os.environ.get("ANTHROPIC_API_KEY")
    mode = "LLM (claude-sonnet-4-6)" if api_key else "mock (keyword fallback)"
    print(f"=== ClinicalTrialDataAgent — Q4 Demo ===")
    print(f"Data   : {DATA_PATH}")
    print(f"Mode   : {mode}")
    print("=" * 40)

    agent = ClinicalTrialDataAgent(str(DATA_PATH), api_key=api_key)

    for query in QUERIES:
        result = agent.ask(query)
        print_result(result)

    print()


if __name__ == "__main__":
    main()
