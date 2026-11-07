
## LiChess Bots

Registering a bot account on Lichess is a straightforward process, but it requires creating a **dedicated account** and upgrading it via the official Lichess API.

Here are the step-by-step instructions and requirements involved:

---

### Key Requirements Before Starting

* **Dedicated Account:** You must register a **brand new account** specifically for your bot. You cannot convert an existing account if it has already played any human games.
* **No Human Play:** The new account must have zero played games before you upgrade it to a bot.
* **Irreversible:** Once upgraded to a Bot account, the action cannot be undone. The account can never be converted back into a normal human account.

---

### Step-by-Step Registration Process

#### 1. Create a New Lichess Account

1. Log out of your main account and register a new account on [Lichess.org](https://lichess.org).
2. Choose a username that clearly indicates it is a bot (e.g., `MyEngine_Bot`).
3. Confirm the email address to activate the account.

#### 2. Generate a Personal API Access Token

1. Log into the newly created bot account.
2. Go to **Settings** $\to$ **API Access Tokens** (or visit `[https://lichess.org/account/oauth/token](https://lichess.org/account/oauth/token)`).
3. Click **Generate a new token**.
4. Set a description (e.g., `Bot API Token`).
5. Select the required OAuth scopes:
* **Play games with the bot API** (`bot:play`)
* **Read incoming challenges** (`challenge:read`)
* **Create, accept, decline challenges** (`challenge:write`)


6. Click **Generate** and copy your token string immediately.

#### 3. Upgrade the Account to a Bot Status

To upgrade your account, send an HTTP POST request to the Lichess Bot API using your access token. You can easily do this via `curl` in your terminal:

```bash
curl -X POST https://lichess.org/api/bot/account/upgrade \
     -H "Authorization: Bearer YOUR_API_TOKEN_HERE"

```

If successful, Lichess will respond with `{"ok":true}`, and a purple **BOT** badge will appear next to your username on Lichess.

---

### Running Your Engine as a Bot

Once upgraded, your account needs a client program to interface between Lichess's HTTP/NDJSON stream API and your engine (typically via the UCI protocol).

You don't need to write this bridge yourself—you can use existing open-source Python connectors:

* **`lichess-bot` (Official Python Client):** The most popular tool. You simply configure a `config.yml` file with your API token and the local file path to your compiled engine executable (e.g., your Fortran binary or Stockfish).
* The bridge handles accepting challenges, streaming board state changes to your engine via UCI, and sending move responses back to Lichess automatically.

---

### Special Rules for Bot Accounts

* **Bot vs. Bot & Willing Humans:** Your bot can accept challenges from other bots or from human players who explicitly challenge it.
* **Automatic Pairing:** Bots can join the Lichess Bot matchmaking pool to automatically play matches against other online bots.
* **No Automatic Rated Human Pairing:** Bots are excluded from general human Quick Pairing / Matchmaking queues to prevent accidental unagreed play against humans.
