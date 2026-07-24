import { randomUUID } from "node:crypto";
import { eq, sql } from "drizzle-orm";
import { afterAll, beforeEach, describe, expect, it } from "vitest";
import { getAuth } from "@/lib/auth";
import { getDatabase, getPool } from "@/lib/db";
import { account, session, user } from "@/lib/db/schema";

/**
 * Chicago TDD: the real better-auth server API (auth.api.signUpEmail/
 * signInEmail) against the real local Postgres, asserting on the real rows
 * it writes -- not on mocked responses.
 */

const db = getDatabase();

beforeEach(async () => {
  await db.execute(sql`TRUNCATE TABLE "user" CASCADE`);
});

afterAll(async () => {
  await getPool().end();
});

function freshEmail() {
  return `auth-${randomUUID()}@example.invalid`;
}

describe("sign-up writes real user + account rows", () => {
  it("creates a real user row and a credential account row on successful sign-up", async () => {
    const auth = getAuth();
    const email = freshEmail();
    const result = await auth.api.signUpEmail({ body: { name: "Real User", email, password: "Aa1!aaaaaaaa" } });

    expect(result.user.email).toBe(email);

    const userRows = await db.select().from(user).where(eq(user.email, email));
    expect(userRows).toHaveLength(1);
    expect(userRows[0]?.id).toBe(result.user.id);

    const accountRows = await db.select().from(account).where(eq(account.userId, result.user.id));
    expect(accountRows).toHaveLength(1);
    expect(accountRows[0]?.providerId).toBe("credential");
    // A real password hash was stored -- not the plaintext password.
    expect(accountRows[0]?.password).toBeTruthy();
    expect(accountRows[0]?.password).not.toBe("Aa1!aaaaaaaa");
  });

  it("refuses a duplicate email via the real unique constraint path", async () => {
    const auth = getAuth();
    const email = freshEmail();
    await auth.api.signUpEmail({ body: { name: "First", email, password: "Aa1!aaaaaaaa" } });

    await expect(auth.api.signUpEmail({ body: { name: "Second", email, password: "Bb2!bbbbbbbb" } })).rejects.toThrow();

    const rows = await db.select().from(user).where(eq(user.email, email));
    expect(rows).toHaveLength(1);
  });

  it("refuses a password shorter than the real minimum", async () => {
    const auth = getAuth();
    const email = freshEmail();
    await expect(auth.api.signUpEmail({ body: { name: "Short", email, password: "short1" } })).rejects.toThrow();

    const rows = await db.select().from(user).where(eq(user.email, email));
    expect(rows).toHaveLength(0);
  });
});

describe("sign-in against a real credential row", () => {
  it("succeeds with the correct password and creates a real session row", async () => {
    const auth = getAuth();
    const email = freshEmail();
    const password = "Aa1!aaaaaaaa";
    await auth.api.signUpEmail({ body: { name: "Signer", email, password } });

    const result = await auth.api.signInEmail({ body: { email, password } });
    expect(result.token).toBeTruthy();

    const sessionRows = await db.select().from(session).where(eq(session.token, result.token));
    expect(sessionRows).toHaveLength(1);
    expect(sessionRows[0]?.userId).toBe(result.user.id);
  });

  it("refuses the wrong password and creates no additional session", async () => {
    const auth = getAuth();
    const email = freshEmail();
    // signUpEmail auto-signs-in by default, so one real session row already
    // exists at this point -- the assertion is that a failed sign-in attempt
    // does not add another, not that the count is zero.
    await auth.api.signUpEmail({ body: { name: "Signer", email, password: "Aa1!aaaaaaaa" } });
    const before = await db.select().from(session).innerJoin(user, eq(session.userId, user.id)).where(eq(user.email, email));

    await expect(auth.api.signInEmail({ body: { email, password: "WrongPassword1!" } })).rejects.toThrow();

    const after = await db.select().from(session).innerJoin(user, eq(session.userId, user.id)).where(eq(user.email, email));
    expect(after).toHaveLength(before.length);
  });
});
