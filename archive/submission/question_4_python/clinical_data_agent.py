"""
clinical_data_agent.py
----------------------
GenAI Clinical Data Assistant for Roche ADS Programmer Assessment — Question 4.

Translates natural-language clinical questions into Pandas DataFrame filters
using Claude (claude-sonnet-4-6) via the Anthropic API, with a keyword-based
mock fallback when the API is unavailable.

Usage
-----
    from clinical_data_agent import ClinicalTrialDataAgent

    agent = ClinicalTrialDataAgent("data/adae.csv")
    result = agent.ask("Which patients experienced dizziness?")
    print(result)
"""

from __future__ import annotations

import json
import os
import re
from pathlib import Path
from typing import Optional

import pandas as pd


# ---------------------------------------------------------------------------
# Schema description passed to the LLM as context
# ---------------------------------------------------------------------------
SCHEMA_DESCRIPTION = """
You are a clinical data query assistant. The dataset is an ADaM ADAE
(Adverse Event) dataset with the following key columns:

  USUBJID   - Unique subject identifier (e.g. "01-701-1015")
  AETERM    - Reported term for the adverse event (e.g. HEADACHE, DIZZINESS)
  AEDECOD   - Dictionary-derived term (standardised MedDRA preferred term)
  AESOC     - Primary System Organ Class (e.g. CARDIAC DISORDERS,
               SKIN AND SUBCUTANEOUS TISSUE DISORDERS)
  AESEV     - Severity / intensity of AE (values: MILD, MODERATE, SEVERE)
  TRTEMFL   - Treatment-emergent flag (Y = treatment-emergent, N = not)

Your job: given a natural-language question, identify the single most relevant
column and filter value, then return ONLY valid JSON in this exact format:

{"target_column": "COLUMN_NAME", "filter_value": "VALUE"}

Rules:
- Use column names exactly as listed above (uppercase).
- Use filter values in UPPERCASE to match the dataset.
- Return nothing else — no explanation, no markdown, just the JSON object.
"""

# ---------------------------------------------------------------------------
# Keyword rules used by the mock fallback
# ---------------------------------------------------------------------------
_SEVERITY_KEYWORDS = {"severity", "sever", "intense", "intensity", "mild",
                      "moderate", "severe"}
_SOC_KEYWORDS = {
    "cardiac": "CARDIAC DISORDERS",
    "heart": "CARDIAC DISORDERS",
    "skin": "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
    "nervous": "NERVOUS SYSTEM DISORDERS",
    "neural": "NERVOUS SYSTEM DISORDERS",
    "neurological": "NERVOUS SYSTEM DISORDERS",
    "gastro": "GASTROINTESTINAL DISORDERS",
    "gi ": "GASTROINTESTINAL DISORDERS",
    "intestin": "GASTROINTESTINAL DISORDERS",
    "musculo": "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS",
    "muscle": "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS",
    "respir": "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",
    "lung": "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",
    "infect": "INFECTIONS AND INFESTATIONS",
    "general": "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
    "vascular": "VASCULAR DISORDERS",
    "metabol": "METABOLISM AND NUTRITION DISORDERS",
    "psychiatr": "PSYCHIATRIC DISORDERS",
    "renal": "RENAL AND URINARY DISORDERS",
    "urinary": "RENAL AND URINARY DISORDERS",
    "eye": "EYE DISORDERS",
    "ocular": "EYE DISORDERS",
    "ear": "EAR AND LABYRINTH DISORDERS",
    "immune": "IMMUNE SYSTEM DISORDERS",
    "blood": "BLOOD AND LYMPHATIC SYSTEM DISORDERS",
    "lymph": "BLOOD AND LYMPHATIC SYSTEM DISORDERS",
    "hepat": "HEPATOBILIARY DISORDERS",
    "liver": "HEPATOBILIARY DISORDERS",
    "endocrin": "ENDOCRINE DISORDERS",
    "reprod": "REPRODUCTIVE SYSTEM AND BREAST DISORDERS",
    "congen": "CONGENITAL, FAMILIAL AND GENETIC DISORDERS",
    "neoplas": "NEOPLASMS BENIGN, MALIGNANT AND UNSPECIFIED",
    "surgical": "SURGICAL AND MEDICAL PROCEDURES",
    "social": "SOCIAL CIRCUMSTANCES",
    "injury": "INJURY, POISONING AND PROCEDURAL COMPLICATIONS",
    "invest": "INVESTIGATIONS",
}
_SEVERITY_VALUES = {"mild", "moderate", "severe"}


class ClinicalTrialDataAgent:
    """
    Translates natural-language clinical questions into Pandas filters.

    Parameters
    ----------
    data_path : str
        Path to the ADAE CSV file.
    api_key : str, optional
        Anthropic API key. Falls back to the ANTHROPIC_API_KEY environment
        variable. If neither is set, mock_llm is used automatically.
    """

    def __init__(self, data_path: str, api_key: Optional[str] = None) -> None:
        self.data_path = Path(data_path)
        if not self.data_path.exists():
            raise FileNotFoundError(f"Data file not found: {self.data_path}")

        self.df = pd.read_csv(self.data_path, low_memory=False)

        # Resolve API key
        self.api_key = api_key or os.environ.get("ANTHROPIC_API_KEY")
        self._anthropic_client = None
        if self.api_key:
            try:
                import anthropic  # noqa: PLC0415
                self._anthropic_client = anthropic.Anthropic(api_key=self.api_key)
            except ImportError:
                print("[WARNING] 'anthropic' package not installed — using mock LLM.")

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def ask(self, question: str) -> dict:
        """
        Main entry point: parse a natural-language question and return results.

        Parameters
        ----------
        question : str
            A natural-language clinical question.

        Returns
        -------
        dict
            {
              "question": str,
              "parsed":   {"target_column": str, "filter_value": str},
              "count":    int,
              "subject_ids": list[str],
            }
        """
        parsed = self.parse_question(question)
        results = self.execute_query(parsed)
        return {
            "question": question,
            "parsed": parsed,
            **results,
        }

    def parse_question(self, question: str) -> dict:
        """
        Use the LLM (or mock) to extract a column/value filter from a question.

        Parameters
        ----------
        question : str
            Natural-language question about the ADAE data.

        Returns
        -------
        dict
            {"target_column": str, "filter_value": str}
        """
        if self._anthropic_client is not None:
            try:
                return self._call_llm(question)
            except Exception as exc:  # noqa: BLE001
                print(f"[WARNING] LLM call failed ({exc}), falling back to mock.")

        return self._mock_llm(question)

    def execute_query(self, parsed: dict) -> dict:
        """
        Apply a column/value filter to the ADAE DataFrame.

        Parameters
        ----------
        parsed : dict
            {"target_column": str, "filter_value": str}

        Returns
        -------
        dict
            {"count": int, "subject_ids": list[str]}
        """
        col = parsed.get("target_column", "")
        val = parsed.get("filter_value", "")

        if col not in self.df.columns:
            return {
                "count": 0,
                "subject_ids": [],
                "error": f"Column '{col}' not found in dataset.",
            }

        mask = self.df[col].astype(str).str.upper() == str(val).upper()
        matched = self.df.loc[mask, "USUBJID"].dropna().unique().tolist()

        return {
            "count": len(matched),
            "subject_ids": sorted(matched),
        }

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    def _call_llm(self, question: str) -> dict:
        """Call the Anthropic API and parse the JSON response."""
        message = self._anthropic_client.messages.create(
            model="claude-sonnet-4-6",
            max_tokens=256,
            system=SCHEMA_DESCRIPTION,
            messages=[{"role": "user", "content": question}],
        )
        raw = message.content[0].text.strip()
        # Extract JSON even if the model adds surrounding whitespace/backticks
        json_match = re.search(r"\{[^{}]+\}", raw)
        if not json_match:
            raise ValueError(f"LLM returned non-JSON response: {raw!r}")
        return json.loads(json_match.group())

    def _mock_llm(self, question: str) -> dict:
        """
        Keyword-based fallback that mimics what the LLM would return.

        Rules (in priority order):
        1. Severity keywords  → AESEV  (extract MILD / MODERATE / SEVERE)
        2. SOC body-system keywords → AESOC  (map to full SOC string)
        3. Specific condition terms → AETERM (use the most specific word)
        """
        q_lower = question.lower()

        # --- 1. Severity ---
        if any(kw in q_lower for kw in _SEVERITY_KEYWORDS):
            for sev in _SEVERITY_VALUES:
                if sev in q_lower:
                    return {"target_column": "AESEV", "filter_value": sev.upper()}
            # Generic severity mention without a specific level
            return {"target_column": "AESEV", "filter_value": "MODERATE"}

        # --- 2. Body system (SOC) ---
        for kw, soc_value in _SOC_KEYWORDS.items():
            if kw in q_lower:
                return {"target_column": "AESOC", "filter_value": soc_value}

        # --- 3. Specific AE term (AETERM) ---
        # Heuristic: pick the longest "meaningful" word (≥5 chars) that isn't
        # a stop word; treat it as the reported AE term.
        stop_words = {
            "which", "patients", "subjects", "experienced", "adverse",
            "events", "event", "show", "give", "with", "that", "had",
            "have", "the", "are", "for", "and", "who", "were", "those",
            "treatment", "emergent", "reported", "suffered", "about",
        }
        words = re.findall(r"[a-z]+", q_lower)
        candidates = [w for w in words if len(w) >= 5 and w not in stop_words]
        if candidates:
            # Prefer longer words as more specific
            best = max(candidates, key=len)
            return {"target_column": "AETERM", "filter_value": best.upper()}

        # Ultimate fallback
        return {"target_column": "AETERM", "filter_value": "HEADACHE"}
