# Question 4 — GenAI Clinical Data Assistant (Python)

A Python module that translates natural-language clinical questions into
Pandas DataFrame filters using Claude (claude-sonnet-4-6) via the Anthropic API.

## File structure

```
question_4_python/
├── clinical_data_agent.py   # Main agent class
├── run_queries.py           # Demo script with 3 example queries
├── requirements.txt         # Python dependencies
├── export_adae.R            # R script to (re-)export the ADAE dataset
└── data/
    └── adae.csv             # ADAE dataset (1191 rows × 107 cols)
```

## Setup

```bash
pip install -r requirements.txt
```

## Usage

### With Anthropic API key (live LLM)

```bash
export ANTHROPIC_API_KEY="sk-ant-..."
python run_queries.py
```

### Without API key (keyword mock fallback)

```bash
python run_queries.py   # No key needed — mock activates automatically
```

### Programmatic usage

```python
from clinical_data_agent import ClinicalTrialDataAgent

agent = ClinicalTrialDataAgent("data/adae.csv")

result = agent.ask("Which patients had severe adverse events?")
print(result["count"])        # number of unique subjects
print(result["subject_ids"])  # sorted list of USUBJID values
print(result["parsed"])       # {"target_column": "AESEV", "filter_value": "SEVERE"}
```

## Design

| Component | Description |
|-----------|-------------|
| `SCHEMA_DESCRIPTION` | System prompt describing the ADAE columns and instructing the LLM to return only JSON |
| `_call_llm()` | Sends question to `claude-sonnet-4-6`; extracts JSON from response |
| `_mock_llm()` | Keyword-matching fallback: severity → AESEV, body-system → AESOC, other → AETERM |
| `execute_query()` | Case-insensitive Pandas filter; returns unique USUBJID count + list |
| `ask()` | Public method combining `parse_question()` + `execute_query()` |

## Example output

```
=== ClinicalTrialDataAgent — Q4 Demo ===
Data   : data/adae.csv
Mode   : mock (keyword fallback)
========================================

Query   : Give me the subjects who had adverse events of Moderate severity
→ Parsed: {'target_column': 'AESEV', 'filter_value': 'MODERATE'}
→ Results: 127 unique subjects
→ Subject IDs: [01-701-1015, 01-701-1023, ...]

Query   : Which patients experienced Dizziness?
→ Parsed: {'target_column': 'AETERM', 'filter_value': 'DIZZINESS'}
→ Results: 28 unique subjects
→ Subject IDs: [01-701-1023, ...]

Query   : Show me subjects with cardiac system adverse events
→ Parsed: {'target_column': 'AESOC', 'filter_value': 'CARDIAC DISORDERS'}
→ Results: 11 unique subjects
→ Subject IDs: [01-701-1028, ...]
```
