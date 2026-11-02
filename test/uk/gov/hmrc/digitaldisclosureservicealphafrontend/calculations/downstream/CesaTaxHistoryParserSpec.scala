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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json
import uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.model.{CesaPayment, RepaymentSignal}

/** Shapes taken from cesa-stub's `dds-calc-user` test user. */
class CesaTaxHistoryParserSpec extends AnyWordSpec with Matchers:

  private val calendar = Json.parse("""{ "returnYears": [
    { "taxYearEnd": "2018-04-05", "returnIssueDate": "2018-04-06", "onlineDueDate": "2019-01-31", "returnReceivedDate": null },
    { "taxYearEnd": "2017-04-05", "returnIssueDate": "2017-04-06", "onlineDueDate": "2018-01-31", "returnReceivedDate": "2018-01-20" }
  ]}""")

  private val return2017 = Json.parse("""{ "supplementarySchedules": ["individual_tax_form", "employment", "self_employment", "lloyds_underwriters"] }""")

  private val account = Json.parse("""{ "taxYears": [
    { "taxYear": "1617",
      "declaredIncome": { "amount": 34000.00, "currency": "GBP" },
      "taxCharged": { "amount": 4600.00, "currency": "GBP" },
      "taxPaid": { "amount": 3000.00, "currency": "GBP" },
      "outstanding": { "amount": 1600.00, "currency": "GBP" },
      "payments": [ { "date": "2018-01-25", "amount": { "amount": 3000.00, "currency": "GBP" }, "description": "Part balancing payment" } ] },
    { "taxYear": "1718", "declaredIncome": null,
      "taxCharged": { "amount": 4600.00, "currency": "GBP" },
      "taxPaid": { "amount": 2300.00, "currency": "GBP" },
      "outstanding": { "amount": 2300.00, "currency": "GBP" },
      "payments": [] }
  ]}""")

  private val signals = Json.parse("""{
    "enquiries": [ { "taxYear": "1617", "type": "S9A", "status": "OPEN", "openedDate": "2018-09-03", "closedDate": null, "lastDateOfEnquiry": "2019-01-20" } ],
    "noRepaymentSignals": [
      { "code": 5, "taxYear": "1617", "reason": "Customer under formal enquiry (S9A/S12AC)", "department": "Compliance", "setDate": "2018-09-03" },
      { "code": 6, "reason": "Processing error" }
    ]
  }""")

  private val taxpayer = Json.parse("""{ "name": { "title": "Mr", "forename": "Hugh", "surname": "Man" } }""")
  private val trade    = Json.parse("""{ "tradeName": "Hugh Man Decorating" }""")

  "tax year conversion" should:
    "turn CESA dates and short years into the calculation's label" in:
      CesaTaxHistoryParser.taxYearFromDate("2017-04-05") shouldBe Some("2016-17")
      CesaTaxHistoryParser.taxYearFromDate("2014-04-06") shouldBe Some("2014-15")
      CesaTaxHistoryParser.taxYearFromDate("not a date") shouldBe None
      CesaTaxHistoryParser.taxYearFromShort("1617") shouldBe Some("2016-17")
      CesaTaxHistoryParser.taxYearEnd("2016-17") shouldBe Some(2017)

  "build" should:
    "combine calendar, returns, account, signals and names into one history per tax year, newest first" in:
      val h = CesaTaxHistoryParser.build(
        utr         = "dds-calc-user",
        source      = "CESA",
        calendar    = Some(calendar),
        returns     = Map("2016-17" -> return2017),
        account     = Some(account),
        signalsJson = Some(signals),
        trade       = Some(trade),
        taxpayer    = Some(taxpayer)
      )

      h.taxpayerName shouldBe Some("Mr Hugh Man")
      h.tradeName shouldBe Some("Hugh Man Decorating")
      h.years.map(_.taxYear) shouldBe Seq("2017-18", "2016-17")

      val y1617 = h.forYear("2016-17").get
      y1617.returnStatusKey shouldBe "cesa.return.filed"
      y1617.declaredIncomeTypes shouldBe Seq("employment", "selfEmployment")
      y1617.otherSchedules shouldBe Seq("lloyds_underwriters")
      y1617.declaredIncome shouldBe Some(BigDecimal(34000))
      y1617.taxCharged shouldBe Some(BigDecimal(4600))
      y1617.taxPaid shouldBe Some(BigDecimal(3000))
      y1617.outstanding shouldBe Some(BigDecimal(1600))
      y1617.payments shouldBe Seq(CesaPayment(Some("2018-01-25"), BigDecimal(3000), Some("Part balancing payment")))
      y1617.openEnquiries.map(_.enquiryType) shouldBe Seq(Some("S9A"))
      y1617.repaymentSignals.map(_.code) shouldBe Seq(5)

      val y1718 = h.forYear("2017-18").get
      y1718.returnStatusKey shouldBe "cesa.return.notFiled"
      y1718.declaredIncome shouldBe None

      h.otherSignals shouldBe Seq(RepaymentSignal(6, None, "Processing error", None, None))
      h.repaymentSignals.map(_.code) shouldBe Seq(5, 6)
      h.totalCharged shouldBe Some(BigDecimal(9200))
      h.totalPaid shouldBe Some(BigDecimal(5300))
      h.totalOutstanding shouldBe Some(BigDecimal(3900))

    "mark a year CESA only knows from its account as having no return issued" in:
      CesaTaxHistoryParser.build("u", "s", account = Some(account)).forYear("2017-18").get.returnStatusKey shouldBe
        "cesa.return.notIssued"

    "give an empty history when CESA holds nothing" in:
      val h = CesaTaxHistoryParser.build("u", "s")
      h.hasRecord shouldBe false
      h.totalPaid shouldBe None
