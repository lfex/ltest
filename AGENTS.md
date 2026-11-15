# ltest contributor instructions

Sofie (she/her) coordinates the Operator-authorised ltest planning. The active
source assignment is Project02 Arc01 Slice01, using the dedicated branch
`codex/ltest-project02-arc01` in this source worktree, based on published ltest
0.14.0 at `ed6ada7f7339387a44105cc3e5909a5f69a31e91`. The prior Project01
branch `codex/ltest-first-library` and its records remain preserved. The owner
checkout `/Users/oubiwann/lab/lfe/ltest` and its index are separate and must
remain untouched.

Canonical active planning lives at
`/Users/oubiwann/lab/lfe/ltest/.worktrees/planning/project02-issue-backlog`.
CDC owns that planning worktree and its index. CC owns source and index
mutations in this worktree only while executing the active dispatched prompt.
Do not edit planning ledgers or acceptance files from source work. Follow the
active slice's exact source allowlist, validation and evidence requirements;
preserve unrelated work and stop for CDC if source scope or a binding contract
needs amendment. No push, merge, tag, publication or external issue mutation is
authorized by the Slice01 assignment.

Apply the collaboration-framework and Erlang-guidelines skills for engineering
work, and the accepted shared LFE documentation guidance at pin
`4a691d3f8fcca36d661ae9575a91c351507565a8`, read with:

```sh
git -C /Users/oubiwann/lab/lfe/lfe-manual show \
  4a691d3f8fcca36d661ae9575a91c351507565a8:src/part7/ai-resources/style-guide.md
```

when writing LFE documentation. Preserve public behavior unless a separately
reviewed decision authorizes a change.

Every assistant-authored source commit must use an actual multiline
commit-message file and include exactly these two consecutive trailers,
separated from the body by a blank line. Verify the parsed result with
`git interpret-trailers --parse <message-file>` and parse the trailers from the
resulting commit as well (`git show -s --format=%B HEAD | git
interpret-trailers --parse`):

```text
Co-authored-by: Codex <noreply@openai.com>
Co-authored-by: Billo AI <ai-engineering@billo.systems>
```
