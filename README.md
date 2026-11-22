# SpendWise
Project Description

SpendWise is a personal budget and expense tracking web application designed to help users organize their finances, monitor spending habits, and work toward their financial goals.

Team Members

[Labi Neha] — [lmn73]

[Nandini Venkatesh] — [nv267]

[Anjali Singh] — [as4429]

[Sam Sah-Nixon] — [ss4484]

[Mina Liu] — [mtl79]

## Part 1: Transaction Management

Implemented the core transaction management functionality, allowing users to manage income and expense records.

### Features

- Create income and expense transaction records.
- Add transactions while preventing duplicate IDs.
- Generate the next available transaction ID.
- Retrieve individual transactions by ID or view all transactions.
- Edit existing transactions while preserving their original IDs.
- Delete transactions by ID.
- Search transactions by description using case-insensitive partial matching.
- Filter transactions by date, category, and transaction type.
- Combine multiple filters in a single query.
- Validate transaction amounts, dates, transaction types, and text fields.

### Transaction Data

Each transaction stores the following information:

- **ID:** Unique identifier for each transaction.
- **Amount:** Monetary value of the transaction.
- **Date:** Date the transaction occurred.
- **Description:** Description of the transaction.
- **Category:** Category assigned to the transaction.
- **Transaction Type:** Income or Expense.

### Implementation Files

The transaction management functionality is organized into the following files:

- **`lib/transaction.ml`**  
  Contains the transaction data type, input parsing and validation, ID generation, and core transaction operations, including adding, editing, deleting, viewing, searching, and filtering.

- **`lib/transaction.mli`**  
  Defines the public interface for the transaction module, specifying the types and functions that other modules can access.

- **`test/test_spendwise.ml`**  
  Contains the test suite for transaction management, checking input parsing, validation, ID generation, and transaction operations.

## Part 2: Monthly Category Budgets

Implemented the core budget logic for setting monthly spending limits by expense category and tracking spending against those limits.

### Features

- Create, edit, and delete monthly spending limits by category.
- Allow one budget per month and category, with case-insensitive category matching.
- Validate the month (`YYYY-MM`), category, and spending limit.
- Calculate total spending from expense transactions in the budget's month and category.
- Calculate the remaining budget by subtracting spending from the limit. A negative balance shows how much the budget was exceeded.
- Return a warning status when spending reaches 80% of the limit, meets the limit exactly, or exceeds it. The browser interface can use this status to display a message.

### Budget Data

Each budget stores the following information:

- **Month:** The budget period in `YYYY-MM` format.
- **Category:** The expense category the limit applies to.
- **Limit:** The maximum planned spending for that category and month.

### Implementation Files

- **`lib/budget.ml`**  
  Implements budget validation, creation, lookup, editing, deletion, and spending summaries using the existing transaction records.

- **`lib/budget.mli`**  
  Defines the public budget interface, including budget operations, spending summaries, and warning statuses.

## Part 3: Savings Goals (Mina)

Implemented the core savings goal functionality and browser controls, allowing users to manage multiple goals and track their contributions and progress.

### Features

- Create, edit, and delete multiple savings goals.
- Set a goal name, target amount, and optional deadline.
- Record contributions toward each goal and view contribution history.
- Display progress bars showing the percentage completed, amount saved, and amount remaining.
- Preserve contribution history when editing a goal.
- Allow contributions above the target while capping displayed progress at 100% and remaining at zero.
- Validate goal names, amounts, and deadlines, and use whole cents internally to avoid decimal completion errors.

### Savings Goal Data

Each savings goal stores the following information:

- **ID:** Unique identifier for the goal within the user's goal list.
- **Name:** The financial objective the user is saving toward.
- **Target Amount:** The amount the user intends to save.
- **Deadline:** An optional target date in `YYYY-MM-DD` format.
- **Contributions:** The history of amounts contributed toward the goal.

The module calculates the total saved, remaining amount, completion percentage, and completion status from the target and contributions.

### Implementation Files

The savings goal functionality is organized into the following files:

- **`lib/savings.ml`**\
  Implements pure savings logic independent of browser and storage, including validation, creation, lookup, editing, deletion, contributions, and progress calculations.

- **`lib/savings.mli`**\
  Defines the public savings interface and validation rules, using an abstract goal type to protect contribution history and saved totals.

- **`bin/main.ml`**\
  Connects the savings module to browser input handling and renders goal cards, contribution history, and progress bars.

- **`public/index.template.html` and `public/style.css`**\
  Provide the savings goal form, page structure, and styling.

- **`test/test_mina.ml`**\
  Contains OUnit tests for transaction parser edge cases, savings goal behavior, and summary integration.

- **`docs/mina-ms2.txt`**\
  Records report material, validation evidence, and handoff notes for storage and dashboard integration.

### Current Limitations and Integration

Goals currently live in memory and are cleared on page reload. Account-specific persistence belongs to the storage/account component and is not implemented here. Contributions record allocations separately; they do not create expenses or deduct an account balance. The dashboard should consume `Savings.progress` without counting contributions again as spending.

### Setup and checks

With the course OCaml/OPAM environment active:

```sh
opam install . --deps-only --with-test
dune build
dune test
```

Dependencies, including the existing browser libraries and OUnit2 for the new test
suite, are declared in `dune-project`; `spendwise.opam` is generated from it.

To try the browser page, run the following from the repository root after building:

```sh
python3 -m http.server 8765 --bind 127.0.0.1 --directory _build/default/public
```

Open http://127.0.0.1:8765/ and use the savings form. Stop the server with Ctrl-C.

## Part 4: Dashboard & Financial Analytics

Implemented the core analytical calculations and period summaries for the financial dashboard, connecting transaction history and budget limits to provide high-level spending insights.

### Features

- Compute overall financial totals: total income, total expenses, and net remaining balance across all transactions.
- Filter transactions dynamically across time periods (weekly 7-day rolling window, monthly by calendar month, and yearly by calendar year) relative to an anchor date.
- Generate period summaries including period labels, high-level financial overviews, and sorted expense breakdowns.
- Break down expense spending by category, calculating exact expenditure amounts and percentage shares of total spending, sorted from highest to lowest.
- Track chronological monthly spending trends grouped by year and month (`YYYY-MM`).
- Connect active monthly budgets with real transaction data, displaying spending summaries and warning statuses (approaching limit, at limit, over limit).

### Analytics Data

The analytics module introduces and works with the following data representations:

- **Period:** The time window for financial summaries (`Weekly`, `Monthly`, or `Yearly`).
- **Overview:** Aggregate monetary metrics (`total_income`, `total_expenses`, and `remaining_balance`).
- **Category Breakdown:** Specific category spending details (`category`, `amount`, and `percentage`).
- **Period Summary:** Combined report for a chosen time period containing the period type, formatted label, financial overview, and sorted category breakdowns.

### Implementation Files

The dashboard and financial analytics functionality is organized into the following files:

- **`lib/analytics.ml`**  
  Implements total calculations, deterministic period filtering, category breakdowns, monthly spending trends, and budget summary integrations.

- **`lib/analytics.mli`**  
  Defines the public interface for the analytics module, specifying types for reporting periods, overviews, breakdowns, and summary functions.

- **`test/test_spendwise.ml`**  
  Contains automated tests verifying overview calculations, period filtering boundaries, category expense distributions, and monthly spending trends.
