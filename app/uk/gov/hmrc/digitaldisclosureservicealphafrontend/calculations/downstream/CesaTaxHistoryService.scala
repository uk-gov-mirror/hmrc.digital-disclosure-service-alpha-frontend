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

package uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.downstream

import com.google.inject.ImplementedBy
import play.api.libs.json.JsValue
import uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.model.CesaTaxHistory
import uk.gov.hmrc.digitaldisclosureservicealphafrontend.connectors.{CesaCall, CesaConnector}
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

/**
  * Where a taxpayer's Self Assessment history comes from. Today that is CESA (cesa-stub locally); after the
  * CESA -> ETMP migration it would be the Individual Self Assessment Liabilities API, behind this same interface.
  */
@ImplementedBy(classOf[CesaTaxHistoryService])
trait TaxHistorySource:
  def sourceLabel: String

  /** Left only when the source could not be reached at all. An unknown UTR gives an empty history. */
  def fetch(utr: String, taxYears: Seq[String])(using HeaderCarrier): Future[Either[String, CesaTaxHistory]]

/** Fetches a UTR's history from CESA: calendar, returns, charged/paid/owed per year, signals and names. */
@Singleton
class CesaTaxHistoryService @Inject() (connector: CesaConnector)(using ec: ExecutionContext) extends TaxHistorySource:

  def sourceLabel: String = s"CESA at ${connector.baseUrl}"

  def fetch(utr: String, taxYears: Seq[String])(using HeaderCarrier): Future[Either[String, CesaTaxHistory]] =
    val rootF     = connector.individual(utr)
    val calendarF = connector.calendar(utr)
    val accountF  = connector.individualResource(utr, "Tax years", "account/tax-years")
    val signalsF  = connector.individualResource(utr, "Signals", "signals")
    val tradeF    = connector.individualResource(utr, "Trade", "designatory-details/trade")
    val taxpayerF = connector.individualResource(utr, "Taxpayer", "designatory-details/taxpayer")
    for
      root     <- rootF
      calendar <- calendarF
      account  <- accountF
      signals  <- signalsF
      trade    <- tradeF
      taxpayer <- taxpayerF
      returns  <- fetchReturns(utr, yearsToCheck(calendar, taxYears))
    yield
      if Seq(root, calendar, account, signals, trade, taxpayer).forall(_.status.isEmpty) then
        Left(root.error.getOrElse("Could not reach CESA"))
      else
        Right(
          CesaTaxHistoryParser
            .build(
              utr         = utr,
              source      = sourceLabel,
              calendar    = successJson(calendar),
              returns     = returns,
              account     = successJson(account),
              signalsJson = successJson(signals),
              trade       = successJson(trade),
              taxpayer    = successJson(taxpayer)
            )
            .copy(knownTaxpayer = root.isSuccess)
        )

  /** Every year in the CESA calendar plus the years asked for. */
  private def yearsToCheck(calendar: CesaCall, taxYears: Seq[String]): Seq[String] =
    (successJson(calendar).map(CesaTaxHistoryParser.calendarYears).getOrElse(Nil).map(_.taxYear) ++ taxYears).distinct

  private def fetchReturns(utr: String, taxYears: Seq[String])(using HeaderCarrier): Future[Map[String, JsValue]] =
    Future
      .traverse(taxYears): taxYear =>
        CesaTaxHistoryParser.taxYearEnd(taxYear) match
          case Some(end) => connector.taxReturn(utr, end).map(call => successJson(call).map(taxYear -> _))
          case None      => Future.successful(None)
      .map(_.flatten.toMap)

  private def successJson(call: CesaCall): Option[JsValue] =
    if call.isSuccess then call.json else None
