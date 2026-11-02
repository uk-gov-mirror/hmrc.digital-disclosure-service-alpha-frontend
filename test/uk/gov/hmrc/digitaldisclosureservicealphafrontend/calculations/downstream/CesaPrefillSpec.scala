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
import uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.CalculationsFixtures.*
import uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.engine.LiabilityCalculator
import uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.model.{CesaTaxHistory, CesaTaxYear}

class CesaPrefillSpec extends AnyWordSpec with Matchers:

  private val history = CesaTaxHistory(
    utr          = "dds-calc-user",
    source       = "CESA",
    taxpayerName = Some("Mr Hugh Man"),
    tradeName    = Some("Hugh Man Decorating"),
    years        = Seq(
      CesaTaxYear("2017-18", taxCharged = Some(BigDecimal(4600)), taxPaid = Some(BigDecimal(2300))),
      CesaTaxYear("2015-16", declaredIncome = Some(BigDecimal("32000.00")), taxCharged = Some(BigDecimal(4280)), taxPaid = Some(BigDecimal("4280.00")))
    )
  )

  "CesaPrefill.answers" should:
    "fill declared income, tax paid and business name for each calculation year CESA has figures for" in:
      CesaPrefill.answers(history, Seq("2015-16", "2016-17", "2017-18")) shouldBe Map(
        "alreadyDeclaredIncome__2015-16"      -> "32000",
        "taxAlreadyPaid__2015-16"             -> "4280",
        "taxAlreadyPaid__2017-18"             -> "2300",
        "selfEmploymentBusinessName__2015-16" -> "Hugh Man Decorating",
        "selfEmploymentBusinessName__2016-17" -> "Hugh Man Decorating",
        "selfEmploymentBusinessName__2017-18" -> "Hugh Man Decorating"
      )

  "A calculation using CESA's figures" should:
    "charge the extra tax on top of what was paid: tax on (declared + disclosed income) minus tax paid" in:
      // 2015-16: £32,000 declared + £10,000 disclosed = £42,000. Less £10,600 allowance = £31,400 at 20% = £6,280.
      // CESA shows £4,280 paid, so the extra tax is £2,000 (20% of the £10,000 disclosed).
      val answers = aboutYou ++ Map(
        "taxYears"                        -> "2015-16",
        "incomeTypes"                     -> "selfEmployment",
        "selfEmploymentTurnover__2015-16" -> "10000"
      ) ++ CesaPrefill.answers(history, Seq("2015-16"))

      val result = LiabilityCalculator.calculate(catalog, spec, answers)
      result.years.map(_.taxYear) shouldBe Seq("2015-16")
      result.years.head.totalIncome shouldBe BigDecimal(42000)
      result.years.head.taxDue shouldBe BigDecimal(2000)
