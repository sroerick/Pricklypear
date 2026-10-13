-- Pricklypear migration 005: durable agent conversation store.
--
-- Per spec [ai-authoring.agent-portal]:
--   One tool-capable agent loop. Provider chat/completions is
--   stateless — every turn resubmits full history. We own durability
--   here in ai_sessions + ai_messages.
--
-- Named to avoid clash with:
--   sessions      (fleet / image instances, migration 001)
--   auth_sessions (login cookies, migration 002)
--
-- Additive-only. System seed lives on ai_sessions, not as role=system
-- message rows. Message roles: user | assistant | tool.

CREATE TABLE IF NOT EXISTS ai_sessions (
  id          uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  owner       text NOT NULL,              -- username (whoami)
  title       text,                       -- optional; often first user line
  seed        text NOT NULL,              -- system prompt; not a message row
  branch_ref  text,                       -- branch at create / last say
  tools       boolean NOT NULL DEFAULT true,
  model       text,                       -- LLM_MODEL snapshot at create
  created_at  timestamptz NOT NULL DEFAULT now(),
  updated_at  timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS ai_sessions_owner_updated_idx
  ON ai_sessions (owner, updated_at DESC);

CREATE TABLE IF NOT EXISTS ai_messages (
  id            uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  session_id    uuid NOT NULL REFERENCES ai_sessions(id) ON DELETE CASCADE,
  seq           int  NOT NULL,            -- dense order from 0
  role          text NOT NULL,            -- user | assistant | tool
  content       text,                     -- nullable for pure tool-call turns
  tool_calls    jsonb,                    -- [{id,name,arguments}] or null
  tool_call_id  text,                     -- set when role=tool
  created_at    timestamptz NOT NULL DEFAULT now(),
  UNIQUE (session_id, seq),
  CONSTRAINT ai_messages_role_chk
    CHECK (role IN ('user', 'assistant', 'tool'))
);

CREATE INDEX IF NOT EXISTS ai_messages_session_seq_idx
  ON ai_messages (session_id, seq);

INSERT INTO schema_migrations (version, description)
SELECT 5, 'agent conversation store (ai_sessions, ai_messages)'
WHERE NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 5);
