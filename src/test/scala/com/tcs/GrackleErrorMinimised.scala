package com.tcs

import cats.effect.IO
import cats.syntax.all._
import grackle._
import grackle.syntax._
import io.circe.Json
import io.circe.syntax.EncoderOps
import munit.CatsEffectSuite

class GrackleErrorMinimised extends CatsEffectSuite {

  test("grackle throws a StackOverflow") {
    commonTest("quux") // same value as the id of the child object
  }

  test("grackle is ok") {
    commonTest("anythingElse")
  }

  def commonTest(intermediateFieldName: String): IO[Unit] = {
    val fooJson = Json.arr(
      Json.obj(("quux", Json.fromString("myValue1")))
    )

    val barJsonResult: Result[Json] = Result.success(Json.obj((intermediateFieldName, "myValue1".asJson)))

    object M extends TestMapping {
      val schema =
        schema"""
          type Query {
            foo: [Foo!]!
          }
          type Foo {
            id: Bar
          }
          type Bar {
            quux: String
          }
        """
      override val typeMappings: List[TypeMapping] = List(
        ObjectMapping(
          tpe = schema.ref("Query"),
          fieldMappings = List(
            CirceField("foo", fooJson)
          )
        ),
        ObjectMapping(
          tpe = schema.ref("Foo"),
          fieldMappings = List(
            CursorFieldJson("id", _ => barJsonResult, Nil)
          )
        ),
        ObjectMapping(
          tpe = schema.ref("Bar"),
          fieldMappings = List(
            CursorFieldJson("quux", _.fieldAs[Json](intermediateFieldName), Nil)
          )
        )
      )
    }

    // validate mapping first
    val es = M.validator.validateMapping()
    es match {
      case Nil => ()
      case _   => fail(es.foldMap(_.toErrorMessage))
    }

    val query = """query {
          foo {
            id {
              quux
            }
          }
      }
      """

    // then run a basic query
    val res: IO[Json] = M.compileAndRun(query)

    val expectedResult =
      Json.obj(
        "data" -> Json.obj(
          "foo" -> Json.arr(
            Json.obj(
              "id" -> Json.obj(
                ("quux", Json.fromString("myValue1"))
              )
            )
          )
        )
      )

    assertIO(res, expectedResult)
  }
}
