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
## Part 3: Savings goals (Mina)

The browser now supports creating, editing, deleting, and contributing to multiple
savings goals. A goal has a name, target, optional deadline, contribution history,
and progress bar. Editing preserves contributions. Overfunded goals retain the
full saved amount while progress stops at 100% and remaining stops at zero.
Amounts use whole cents internally to avoid decimal completion errors.

- `lib/savings.mli`: public interface and validation rules; abstract goal type.
- `lib/savings.ml`: pure savings logic, independent of browser and storage.
- `bin/main.ml`: browser interaction and rendering using the savings module.
- `test/test_mina.ml`: OUnit parser, savings, and summary integration tests.
- `docs/mina-ms2.txt`: report material, validation evidence, and team handoff.

Goals currently live in memory and are cleared on page reload. Account-specific
persistence belongs to the storage/account component and is not implemented here.
Contributions record allocations separately; they do not create expenses or deduct
an account balance. The dashboard should consume `Savings.progress` without
counting contributions again as spending.

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
