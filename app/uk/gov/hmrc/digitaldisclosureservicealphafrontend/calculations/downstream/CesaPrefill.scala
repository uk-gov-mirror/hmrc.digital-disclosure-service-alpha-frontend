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

import uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.model.{CesaTaxHistory, YearAnswers}

/**
  * The calculation answers CESA can fill in for each of the calculation's tax years:
  *   - `alreadyDeclaredIncome` from the income declared on the return
  *   - `taxAlreadyPaid` from the tax CESA shows as paid ("We can see you paid £X for this year")
  *   - `selfEmploymentBusinessName` from the trade name
  * The calculation spec (v1.6.0+) counts already-declared income, so tax due is tax on (declared + disclosed income)
  * minus the tax already paid.
  */
object CesaPrefill:

  def answers(history: CesaTaxHistory, taxYears: Seq[String]): Map[String, String] =
    taxYears.flatMap { year =>
      val cesaYear = history.forYear(year)
      cesaYear.flatMap(_.declaredIncome).map(v => YearAnswers.key("alreadyDeclaredIncome", year) -> plain(v)).toSeq ++
        cesaYear.flatMap(_.taxPaid).map(v => YearAnswers.key("taxAlreadyPaid", year) -> plain(v)).toSeq ++
        history.tradeName.map(name => YearAnswers.key("selfEmploymentBusinessName", year) -> name).toSeq
    }.toMap

  private def plain(value: BigDecimal): String =
    value.bigDecimal.stripTrailingZeros.toPlainString
