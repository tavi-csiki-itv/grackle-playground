package com.tcs

import cats.MonadThrow
import cats.effect.IO
import grackle.circe.CirceMapping

abstract class TestMapping(implicit val MT: MonadThrow[IO]) extends CirceMapping[IO] {
  val typeMappings: List[TypeMapping] = Nil
}