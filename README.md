# ABAPfy: AI-Powered ABAP Development Assistant

ABAPfy is a command-line interface (CLI) tool designed to streamline ABAP development by leveraging the power of Artificial Intelligence. It assists developers in generating, reviewing, and debugging ABAP code, aiming to improve code quality, reduce development time, and enforce best practices.

---

## Table of Contents

- [About The Project](#about-the-project)
- [Key Features](#key-features)
- [Architecture Overview](#architecture-overview)
- [Getting Started](#getting-started)

  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
  - [Usage](#usage)
  - [Configuration](#configuration)

- [Available Commands](#available-commands)
- [Example Workflow](#example-workflow-generating-an-alv-report)
- [Contributing](#contributing)
- [License](#license)
- [Contact](#contact)

---

## About The Project

ABAP development, while powerful, can often be verbose and time-consuming. ABAPfy addresses this by providing an intelligent assistant that understands the nuances of the ABAP language. By integrating with Large Language Models (LLMs), ABAPfy offers a suite of tools to automate repetitive tasks, ensure code quality, and provide intelligent suggestions.

The core of ABAPfy is a system of specialized AI agents that work in concert to handle different aspects of the development lifecycle: from refining user prompts and selecting appropriate templates to generating and reviewing the final code.

---

## Key Features

- **AI-Powered Code Generation:** Automatically generate complex ABAP programs and code modules from simple natural language descriptions.
- **Intelligent Code Review:** Analyze existing ABAP code to identify potential bugs, performance issues, and deviations from best practices.
- **Automated Debugging:** Assist in pinpointing and fixing errors in buggy ABAP code.
- **Template-Based Development:** Utilizes a rich library of predefined templates for common ABAP patterns and objects like ALV reports, BAdI implementations, and BAPI wrappers.
- **Context-Aware Assistance:** Employs embedding techniques to understand the context of your existing codebase for more relevant and accurate suggestions.
- **Interactive CLI:** A user-friendly command-line interface that guides the developer through various development tasks.

---

## Architecture Overview

ABAPFy is built on a modular architecture centered around a multi-agent system. Each agent has a specific role, ensuring a clear separation of concerns and making the system extensible.

- **Agent Orchestrator (`agent_orchestrator.py`):** Central coordinator that manages workflow and communication between agents.
- **Prompt Refiner Agent (`prompt_refiner.py`):** Refines user input into a detailed, structured prompt.
- **Template Selector Agent (`template_selector.py`):** Chooses the most appropriate code template.
- **Code Developer Agent (`code_developer.py`):** Generates the ABAP code.
- **Code Reviewer Agent (`code_reviewer.py`):** Reviews code for quality, correctness, and adherence to best practices.
- **AI Client (`ai/client.py`):** Wrapper around the chosen LLM API (e.g., OpenAI, Gemini).
- **Template Manager (`templates/manager.py`):** Handles loading and processing of templates.
- **Embeddings Manager (`embeddings/manager.py`):** Manages vector embeddings for semantic search and context-aware analysis.

---

## Getting Started

Follow these steps to get a local copy up and running.

### Prerequisites

- Python 3.8 or higher
- An API key for an AI provider (e.g., OpenAI, Google AI)

### Installation

```bash
git clone https://github.com/esc4n0rx/abafy-v2.git
cd abafy-v2

# Install dependencies
pip install -r requirements.txt

# Install package in editable mode
pip install -e .
```

### Usage

ABAPfy is run from the command line. The main entry point is `abapfy/main.py`.

### Configuration

Before the first run, configure your AI provider and API key:

```bash
abafy
```

Follow the on-screen instructions to select your AI provider (e.g., Gemini) and enter your API key.

---

## Available Commands

- **Generate Program:** Starts workflow for generating a new ABAP program.
- **Generate Module:** Creates smaller, reusable code modules.
- **Review Code:** Analyzes an existing `.abap` file for issues.
- **Debug Code:** Assists in fixing bugs in existing programs.

---

## Example Workflow: Generating an ALV Report

```bash
abafy
```

1. Select **Generate Program** from the menu.
2. Enter a prompt, for example:

   > "Create an ALV report to display sales order data from tables VBAK and VBAP. The report should show the sales order number, creation date, material number, and quantity. Add a selection screen for the sales order number."

3. The orchestrator processes the request, generates the code, and saves it as `output/report_test.abap`.

---

## Contributing

Contributions are what make the open-source community such an amazing place to learn, inspire, and create. Any contributions you make are greatly appreciated.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

---

## License

Distributed under the MIT License. See `LICENSE` for more information.

---

## Contact

Project Link: [https://github.com/esc4n0rx/abafy-v2](https://github.com/esc4n0rx/abafy-v2)
