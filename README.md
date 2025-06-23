# 🏦 Credstack - Decentralized Credit Scoring

> Build transparent credit profiles on the Stacks blockchain 📊

## 🌟 Overview

Credstack is a decentralized credit scoring system that enables users to build and maintain credit profiles entirely on-chain. Unlike traditional credit systems, Credstack provides full transparency and user control over credit data.

## ✨ Features

- 🆔 **Create Credit Profiles**: Establish your on-chain credit identity
- 💰 **Request Loans**: Borrow based on your credit score
- 📈 **Build Credit History**: Improve your score through timely repayments
- 📊 **Transparent Scoring**: All credit calculations are visible on-chain
- 🔍 **Credit History Tracking**: Complete audit trail of all credit activities
- ⚡ **Real-time Updates**: Credit scores update immediately with each transaction

## 🚀 Getting Started

### Prerequisites
- Clarinet installed
- Stacks wallet

### Installation

```bash
git clone <your-repo>
cd credstack
clarinet console
```

## 📖 Usage

### Creating a Credit Profile

```clarity
(contract-call? .Credstack create-profile)
```

### Requesting a Loan

```clarity
(contract-call? .Credstack request-loan u1000 u144) ;; 1000 STX for 144 blocks
```

### Repaying a Loan

```clarity
(contract-call? .Credstack repay-loan u1 u500) ;; Repay 500 STX on loan #1
```

### Checking Credit Score

```clarity
(contract-call? .Credstack get-credit-score 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

## 🎯 Core Functions

### Public Functions
- `create-profile()` - Initialize your credit profile
- `request-loan(amount, duration)` - Request a loan
- `repay-loan(loan-id, amount)` - Make loan payments
- `mark-loan-default(loan-id)` - Mark overdue loans as defaulted (owner only)
- `update-credit-score(user, score)` - Manual score adjustment (owner only)

### Read-Only Functions
- `get-credit-profile(user)` - Get complete credit profile
- `get-credit-score(user)` - Get current credit score
- `get-loan(loan-id)` - Get loan details
- `get-user-loans(user)` - Get all user's loans
- `get-credit-history(user)` - Get credit activity history
- `calculate-credit-utilization(user)` - Calculate debt utilization ratio
- `get-loan-status(loan-id)` - Check if loan is active/repaid/defaulted
- `is-loan-overdue(loan-id)` - Check if loan is past due

## 📊 Credit Scoring

- **Default Score**: 500
- **Score Range**: 300-850
- **Payment Bonus**: +10 points per loan repaid
- **Default Penalty**: -50 points per defaulted loan
- **Minimum Score for Loans**: 300

## 🔧 Contract Constants

```clarity
MIN_CREDIT_SCORE: 300
MAX_CREDIT_SCORE: 850
DEFAULT_CREDIT_SCORE: 500
SCORE_ADJUSTMENT_PAYMENT: 10
SCORE_ADJUSTMENT_DEFAULT: 50
```

## 🧪 Testing

```bash
clarinet test
```

## 📝 Example Workflow

1. **Create Profile** 🆕
   ```clarity
   (contract-call? .Credstack create-profile)
   ```

2. **Request Loan** 💸
   ```clarity
   (contract-call? .Credstack request-loan u1000 u144)
   ```

3. **Repay Loan** ✅
   ```clarity
   (contract-call? .Credstack repay-loan u1 u1000)
   ```

4. **Check Updated Score** 📈
   ```clarity
   (contract-call? .Credstack get-credit-score tx-sender)
   ```
