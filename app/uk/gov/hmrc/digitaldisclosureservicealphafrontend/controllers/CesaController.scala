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

package uk.gov.hmrc.digitaldisclosureservicealphafrontend.controllers

import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.config.DefaultConfigs
import uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.downstream.TaxHistorySource
import uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.model.ArchitectureOption
import uk.gov.hmrc.digitaldisclosureservicealphafrontend.views.html.CesaLookupPage
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

/**
  * The CESA page: a taxpayer's Self Assessment history before any calculation — past tax charged, what was paid,
  * what is still owed, and any enquiry or No Repayment Signal on their record.
  */
@Singleton
class CesaController @Inject() (
  mcc       : MessagesControllerComponents,
  taxHistory: TaxHistorySource,
  lookupPage: CesaLookupPage
)(using ec: ExecutionContext)
    extends FrontendController(mcc) with I18nSupport:

  // Real UTRs are 10 digits; cesa-stub also uses named test users such as "dds-calc-user".
  private val UtrPattern = "^[A-Za-z0-9-]{1,64}$".r

  /** The tax years the calculation covers, so the page can mark them. */
  private val calculationYears: Seq[String] =
    DefaultConfigs.defaultsFor(ArchitectureOption.RatesAndQuestions).rateCatalog.taxYears

  def lookup(utr: Option[String]): Action[AnyContent] = Action.async:
    implicit request =>
      given HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)

      utr.map(_.trim).filter(_.nonEmpty) match
        case None =>
          Future.successful(Ok(lookupPage(None, None, None, calculationYears)))
        case Some(u) if UtrPattern.matches(u) =>
          taxHistory.fetch(u, calculationYears).map:
            case Right(history) => Ok(lookupPage(Some(u), None, Some(history), calculationYears))
            case Left(error)    => BadGateway(lookupPage(Some(u), Some(error), None, calculationYears))
        case Some(u) =>
          Future.successful(
            BadRequest(lookupPage(Some(u), Some(messagesApi.preferred(request)("cesa.utr.error")), None, calculationYears))
          )
