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

package uk.gov.hmrc.digitaldisclosureservicealphafrontend

import com.github.tomakehurst.wiremock.client.WireMock.{aResponse, get, urlEqualTo}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.downstream.TaxHistorySource
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, StringContextOps}
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.test.WireMockSupport

import scala.concurrent.ExecutionContext.Implicits.global

/** The CESA page and tax history, with WireMock standing in for cesa-stub. */
class CesaTaxHistoryIntegrationSpec
  extends AnyWordSpec
     with Matchers
     with ScalaFutures
     with IntegrationPatience
     with WireMockSupport
     with GuiceOneServerPerSuite:

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(
        "microservice.services.cesa.host" -> "localhost",
        "microservice.services.cesa.port" -> wireMockPort
      )
      .build()

  private given HeaderCarrier = HeaderCarrier()

  private val utr     = "1234567890"
  private val base    = s"/self-assessment/individual/$utr"
  private val pageUrl = s"http://localhost:$port/digital-disclosure-service-alpha-frontend/cesa"
  private val years   = Seq("2015-16", "2016-17", "2017-18")

  private def stub(path: String, status: Int, body: String = ""): Unit =
    wireMockServer.stubFor(
      get(urlEqualTo(path)).willReturn(
        aResponse().withStatus(status).withHeader("Content-Type", "application/json").withBody(body)
      )
    )

  private def stubTaxpayer(): Unit =
    stub(base, 200, s"""{"links":{"accountSummary":"$base/account-summary"}}""")
    stub(s"$base/calendar", 200,
      """{"returnYears":[{"taxYearEnd":"2017-04-05","returnIssueDate":"2017-04-06","onlineDueDate":"2018-01-31","returnReceivedDate":"2018-01-20"}]}""")
    stub(s"$base/return/2017", 200, """{"supplementarySchedules":["individual_tax_form","self_employment"]}""")
    stub(s"$base/account/tax-years", 200,
      """{"taxYears":[{"taxYear":"1617","taxCharged":{"amount":4600,"currency":"GBP"},"taxPaid":{"amount":3000,"currency":"GBP"},"outstanding":{"amount":1600,"currency":"GBP"},"payments":[]}]}""")
    stub(s"$base/signals", 200,
      """{"enquiries":[],"noRepaymentSignals":[{"code":5,"taxYear":"1617","reason":"Customer under formal enquiry (S9A/S12AC)"}]}""")
    stub(s"$base/designatory-details/trade", 200, """{"tradeName":"Alex's Plumbing"}""")
    stub(s"$base/designatory-details/taxpayer", 200, """{"name":{"forename":"Alex","surname":"Example"}}""")

  private def source = app.injector.instanceOf[TaxHistorySource]

  "TaxHistorySource (CESA)" should:
    "fetch charged, paid and owed per year with signals and names" in:
      stubTaxpayer()

      val h = source.fetch(utr, years).futureValue.toOption.get

      h.knownTaxpayer shouldBe true
      h.taxpayerName shouldBe Some("Alex Example")
      h.tradeName shouldBe Some("Alex's Plumbing")
      val year = h.forYear("2016-17").get
      year.returnFiled shouldBe true
      year.declaredIncomeTypes shouldBe Seq("selfEmployment")
      year.taxPaid shouldBe Some(BigDecimal(3000))
      year.outstanding shouldBe Some(BigDecimal(1600))
      h.repaymentSignals.map(_.code) shouldBe Seq(5)

    "give an empty history for a UTR CESA does not know" in:
      Seq("", "/calendar", "/account/tax-years", "/signals", "/designatory-details/trade", "/designatory-details/taxpayer")
        .foreach(p => stub(s"$base$p", 404))

      val h = source.fetch(utr, years).futureValue.toOption.get
      h.hasRecord shouldBe false
      h.knownTaxpayer shouldBe false

  "GET /cesa" should:
    "show the form and test users with no UTR" in:
      val response = app.injector.instanceOf[HttpClientV2].get(url"$pageUrl").execute[HttpResponse].futureValue
      response.status shouldBe 200
      response.body should include("CESA tax history")
      response.body should include("dds-calc-user")

    "show the taxpayer's history by year, with repayment holds" in:
      stubTaxpayer()

      val response = app.injector.instanceOf[HttpClientV2].get(url"$pageUrl?utr=$utr").execute[HttpResponse].futureValue

      response.status shouldBe 200
      response.body should include("Alex Example")
      response.body should include("£3,000.00")
      response.body should include("£1,600.00")
      response.body should include("Repayments are on hold")
      response.body should include("NRS 5")

    "reject a UTR containing unexpected characters" in:
      val response =
        app.injector.instanceOf[HttpClientV2].get(url"$pageUrl?utr=${"../../etc"}").execute[HttpResponse].futureValue
      response.status shouldBe 400
      response.body should include("Enter a UTR using only letters, numbers and hyphens")
