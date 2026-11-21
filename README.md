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

