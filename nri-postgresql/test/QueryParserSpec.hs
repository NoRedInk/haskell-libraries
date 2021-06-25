module QueryParserSpec (tests) where

import qualified Expect
import qualified Postgres.QueryParser as Parser
import Test (Test, describe, test)
import qualified Text

-- The tests below pull from real queries used in this repo. If a particular
-- query not listed here doesn't show up correctly in traces please add it as
-- test case.
tests :: Test
tests =
  describe
    "Postgres.QueryParser"
    [ describe
        "parse SQL"
        [ test "simple select" <| \_ ->
            parseTest
              [ "SELECT hat FROM royalty",
                "WHERE hat IN (\"crown\", \"fedora\", \"cap\")"
              ]
              |> Expect.equal (Parser.QueryMeta "royalty" "SELECT"),
          test "subquery in FROM" <| \_ ->
            parseTest
              [ "SELECT draft_id, title, updated_at, created_at",
                "FROM (SELECT (drafts.get_draft(id)).*",
                "      FROM drafts.deltas",
                "      WHERE id = ANY (${draftIds}::uuid[])",
                "      ) draft_data;"
              ]
              |> Expect.equal (Parser.QueryMeta "draft_data" "SELECT"),
          test "unparseable string" <| \_ ->
            parseTest
              [ "Hello,",
                "World!"
              ]
              |> Expect.equal (Parser.QueryMeta "Hello," "UNKNOWN"),
          test "preceded by WITH statement" <| \_ ->
            parseTest
              [ "WITH answers AS (",
                "  SELECT",
                "    correctness,",
                "    index,",
                "    message::text,",
                "    multiple_dropzone_index",
                "  FROM content_creation.multiple_dropzone_answers",
                "  WHERE template_id = ${templateId}",
                "),",
                "draggables AS (",
                "  SELECT",
                "    answer_index,",
                "    array_agg(draggable_index ORDER BY answer_draggable_index ASC) as draggable_indices",
                "  FROM content_creation.multiple_dropzone_answer_draggables",
                "  WHERE template_id = ${templateId}",
                "  GROUP BY answer_index",
                ")",
                "SELECT",
                "  answers.multiple_dropzone_index,",
                "  answers.index,",
                "  answers.correctness,",
                "  draggables.draggable_indices,",
                "  answers.message",
                "FROM answers",
                "LEFT JOIN draggables",
                "  ON answers.index = draggables.answer_index",
                "ORDER BY answers.index ASC"
              ]
              |> Expect.equal (Parser.QueryMeta "answers" "SELECT"),
          test "simple insert" <| \_ ->
            parseTest
              [ "INSERT INTO drafts.deltas (",
                "  title,",
                "  content,",
                "  parent_id",
                ")",
                "VALUES (",
                "  ${Draft.titleDelta delta},",
                "  ${Draft.contentDelta delta},",
                "  ${maybeParentId}",
                ")",
                "RETURNING id, created_at;"
              ]
              |> Expect.equal (Parser.QueryMeta "drafts.deltas" "INSERT"),
          test "insert preceded by WITH statement" <| \_ ->
            parseTest
              [ "WITH insert_text_blocks AS (",
                "  INSERT INTO tutorials.text_blocks (",
                "    tutorial_id,",
                "    id,",
                "    section_id,",
                "    text",
                "  )",
                "  SELECT",
                "    ${tutorialId},",
                "    unnest(${map textBlockId textBlocks}::bigint[]),",
                "    unnest(${map textBlockSectionId textBlocks}::bigint[]),",
                "    unnest(${map textBlockText textBlocks}::text[])",
                "),",
                "insert_tip_blocks AS (",
                "  INSERT INTO tutorials.tip_blocks (",
                "    tutorial_id,",
                "    id,",
                "    section_id,",
                "    tip",
                "  )",
                "  SELECT",
                "    ${tutorialId},",
                "    unnest(${map tipBlockId tipBlocks}::bigint[]),",
                "    unnest(${map tipBlockSectionId tipBlocks}::bigint[]),",
                "    unnest(${map tipBlockTip tipBlocks}::text[])",
                ")",
                "INSERT INTO tutorials.sections (",
                "  tutorial_id,",
                "  id,",
                "  block_type,",
                "  image,",
                "  next_caption",
                ")",
                "SELECT",
                "  ${tutorialId},",
                "  unnest(${map sectionId sections}::bigint[]),",
                "  unnest(${map sectionBlockType sections}::text[]),",
                "  unnest(${map sectionImage sections}::text[]),",
                "  unnest(${map sectionNextCaption sections}::text[])"
              ]
              |> Expect.equal (Parser.QueryMeta "tutorials.sections" "INSERT"),
          test "simple delete" <| \_ ->
            parseTest
              [ "DELETE FROM todo.todos",
                "WHERE user_id = ${userId}",
                "AND id = ${todoId}"
              ]
              |> Expect.equal (Parser.QueryMeta "todo.todos" "DELETE"),
          test "simple update" <| \_ ->
            parseTest
              [ "UPDATE tutorials.tutorials",
                "  SET archived = true",
                "  WHERE public_id = ${toDBPublicId publicId}"
              ]
              |> Expect.equal (Parser.QueryMeta "tutorials.tutorials" "UPDATE"),
          test "simple truncate" <| \_ ->
            parseTest
              [ "TRUNCATE ONLY roses"
              ]
              |> Expect.equal (Parser.QueryMeta "roses" "TRUNCATE")
        ]
    ]

parseTest :: [Text] -> Parser.QueryMeta
parseTest queryLines =
  Text.join "\n" queryLines
    |> Parser.parse
