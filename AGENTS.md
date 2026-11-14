# ltest contributor instructions

Work on the dedicated first-library branch `codex/ltest-first-library` in this source worktree. The owner checkout/default branch and divergent `1.0` work are separate and must remain untouched. Canonical planning for this project is `/Users/oubiwann/lab/lfe/ltest/.worktrees/planning/project01-docs-maintenance`; ldoc programme planning lives in `/Users/oubiwann/lab/lfe/ldoc/.worktrees/planning/project02-dogfooding`. Do not edit planning ledgers or acceptance files from source work.

The project calls the assistant Sofie (she/her). Apply the collaboration-framework and Erlang-guidelines skills for engineering work, and the accepted shared LFE documentation guidance from pin `4a691d3f8fcca36d661ae9575a91c351507565a8`, read with `git -C /Users/oubiwann/lab/lfe/lfe-manual show 4a691d3f8fcca36d661ae9575a91c351507565a8:src/part7/ai-resources/style-guide.md` when writing LFE documentation. Follow the source scope and evidence requirements in the active slice assignment; preserve public behavior unless a separately reviewed decision authorizes a change.

Every assistant-authored source commit must use an actual multiline commit-message file and include exactly these two consecutive trailers, separated from the body by a blank line. Verify the parsed result with `git interpret-trailers --parse <message-file>`
and parse the trailers from the resulting commit as well (`git show -s
--format=%B HEAD | git interpret-trailers --parse`):

```text
Co-authored-by: Codex <noreply@openai.com>
Co-authored-by: Billo AI <ai-engineering@billo.systems>
```
