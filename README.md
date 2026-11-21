# SpendWise
Project Description

SpendWise is a personal budget and expense tracking web application designed to help users organize their finances, monitor spending habits, and work toward their financial goals.

Team Members

[Labi Neha] — [lmn73]

[Nandini Venkatesh] — [nv267]

[Anjali Singh] — [as4429]

[Sam Sah-Nixon] — [ss4484]

[Mina Liu] — [mtl79]

Part 1: Transaction Management (User Input)

Features:

Create income and expense transaction records.

Add transactions while preventing duplicate IDs.

Generate the next available transaction ID based on existing transactions.

Retrieve an individual transaction by ID or view all transactions.

Edit existing transactions while preserving their original IDs.

Delete transactions by ID.

Search transactions by description (case-insensitive partial matching).

Filter transactions by date, category, and transaction type.

Combine multiple filters in a single query.

Validate transaction amounts, dates, transaction types, and text fields.

Transaction Data:

Each transaction stores:

ID

Amount

Date

Description

Category

Transaction type (Income or Expense)


Testing: run dune test to check the functionalities using the test suite