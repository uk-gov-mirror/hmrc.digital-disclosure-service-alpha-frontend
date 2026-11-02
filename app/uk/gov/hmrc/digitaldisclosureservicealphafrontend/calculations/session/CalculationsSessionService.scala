/*
 * Copyright 2026 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.session

import uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.config.{ConfigValidator, ConfigViolation, DefaultConfigs}
import uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.downstream.{CesaPrefill, DownstreamRateCatalogService}
import uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.model.{
  ArchitectureOption,
  CalculationsConfig,
  CesaTaxHistory,
  DownstreamFetchSummary,
  SessionState
}
import uk.gov.hmrc.http.HeaderCarrier

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

/** Starts prototype sessions from the default config and applies changes to config and answers. */
@Singleton
class CalculationsSessionService @Inject() (
  store          : SessionStore,
  downstreamRates: DownstreamRateCatalogService
)(implicit ec: ExecutionContext):

  def get(id: String): Option[SessionState] = store.get(id)

  def start(option: ArchitectureOption)(using HeaderCarrier): Future[SessionState] =
    defaultConfig(option).map: (config, downstream) =>
      store.save(SessionState(UUID.randomUUID().toString, option, config, downstream = downstream))

  def restoreDefaults(state: SessionState)(using HeaderCarrier): Future[SessionState] =
    defaultConfig(state.option).map: (config, downstream) =>
      store.save(withCesaAnswers(state.copy(config = config, answers = Map.empty, downstream = downstream)))

  /**
    * Validates and applies edited config. Answers are cleared because they may not fit the new journey.
    * Options with fixed questions keep their question pack whatever is submitted.
    */
  def updateConfig(
    state          : SessionState,
    rateJson       : String,
    questionJson   : String,
    calculationJson: String
  ): Either[Seq[ConfigViolation], SessionState] =
    val questions = if state.option.questionsEditable then questionJson else state.config.questionJson
    ConfigValidator
      .validate(rateJson, questions, calculationJson)
      .map(config => store.save(withCesaAnswers(state.copy(config = config, answers = Map.empty))))

  def saveAnswer(state: SessionState, questionId: String, value: String): SessionState =
    store.save(state.copy(answers = state.answers + (questionId -> value)))

  def clearAnswers(state: SessionState): SessionState =
    store.save(withCesaAnswers(state.copy(answers = Map.empty)))

  /** Puts the CESA history on the session and fills in what it knows. Answers already given are kept. */
  def attachCesaHistory(state: SessionState, history: CesaTaxHistory): SessionState =
    val filled = CesaPrefill.answers(history, state.rateCatalog.taxYears)
    store.save(
      state.copy(
        answers = filled ++ state.answers,
        cesa    = Some(history.copy(prefilled = filled.keys.toSeq.sorted))
      )
    )

  /** Answers are cleared on config changes and resets; CESA's are put back, since the history still applies. */
  private def withCesaAnswers(state: SessionState): SessionState =
    state.cesa.fold(state)(h => state.copy(answers = CesaPrefill.answers(h, state.rateCatalog.taxYears) ++ state.answers))

  private def defaultConfig(
    option: ArchitectureOption
  )(using HeaderCarrier): Future[(CalculationsConfig, Option[DownstreamFetchSummary])] =
    val defaults = DefaultConfigs.defaultsFor(option)
    if option.fetchesDownstreamRates then
      downstreamRates.overlay(defaults.rateCatalog).map(rates => defaults.withRateCatalog(rates.catalog) -> Some(rates.summary))
    else Future.successful(defaults -> None)
