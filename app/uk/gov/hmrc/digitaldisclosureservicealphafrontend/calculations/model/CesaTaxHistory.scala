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

package uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.model

/** A payment CESA recorded against a tax year. */
final case class CesaPayment(date: Option[String], amount: BigDecimal, description: Option[String])

/** An enquiry (investigation) signal on a tax year, set from CaseFlow. Valid only if opened by the Last Date of Enquiry. */
final case class CesaEnquiry(
  enquiryType      : Option[String],
  open             : Boolean,
  openedDate       : Option[String],
  closedDate       : Option[String],
  lastDateOfEnquiry: Option[String]
)

/**
  * A No Repayment Signal (NRS 1-7): a hold that stops refunds while it is set, e.g. NRS 4 outstanding debt,
  * NRS 5 formal enquiry. Not to be confused with the Non-Repudiation Store PoC elsewhere in this repo.
  */
final case class RepaymentSignal(
  code      : Int,
  taxYear   : Option[String],
  reason    : String,
  department: Option[String],
  setDate   : Option[String]
):
  def label: String = s"NRS $code"

/** What CESA holds about one tax year, keyed by the calculation's tax-year label (e.g. "2016-17"). */
final case class CesaTaxYear(
  taxYear            : String,
  inCalendar         : Boolean = false,
  returnIssueDate    : Option[String] = None,
  filingDueDate      : Option[String] = None,
  returnReceivedDate : Option[String] = None,
  /** Income types declared on the return, as the calculation's `incomeTypes` values (e.g. `selfEmployment`). */
  declaredIncomeTypes: Seq[String] = Nil,
  /** Return pages with no matching calculation income type (e.g. `lloyds_underwriters`). */
  otherSchedules     : Seq[String] = Nil,
  declaredIncome     : Option[BigDecimal] = None,
  taxCharged         : Option[BigDecimal] = None,
  taxPaid            : Option[BigDecimal] = None,
  outstanding        : Option[BigDecimal] = None,
  payments           : Seq[CesaPayment] = Nil,
  enquiries          : Seq[CesaEnquiry] = Nil,
  repaymentSignals   : Seq[RepaymentSignal] = Nil
):
  def returnFiled: Boolean = returnReceivedDate.isDefined

  /** Message key for the return status: filed, issued but not filed, or no return issued. */
  def returnStatusKey: String =
    if !inCalendar then "cesa.return.notIssued"
    else if returnFiled then "cesa.return.filed"
    else "cesa.return.notFiled"

  def openEnquiries: Seq[CesaEnquiry] = enquiries.filter(_.open)

/** A taxpayer's Self Assessment history from CESA: the past tax, what was paid and what is still owed. */
final case class CesaTaxHistory(
  utr             : String,
  source          : String,
  taxpayerName    : Option[String],
  tradeName       : Option[String],
  /** Newest tax year first. */
  years           : Seq[CesaTaxYear],
  /** Signals CESA holds without a tax year. */
  otherSignals    : Seq[RepaymentSignal] = Nil,
  /** CESA knows the UTR (its root resource answered), even if it holds no history. */
  knownTaxpayer   : Boolean = false,
  /** Answer keys filled in from this history when it was used to start a calculation. */
  prefilled       : Seq[String] = Nil
):
  def forYear(taxYear: String): Option[CesaTaxYear] = years.find(_.taxYear == taxYear)
  def hasRecord: Boolean                             = years.nonEmpty || tradeName.isDefined
  def repaymentSignals: Seq[RepaymentSignal]         = years.flatMap(_.repaymentSignals) ++ otherSignals
  def openEnquiries: Seq[(String, CesaEnquiry)]      = years.flatMap(y => y.openEnquiries.map(y.taxYear -> _))

  private def total(f: CesaTaxYear => Option[BigDecimal]): Option[BigDecimal] =
    val values = years.flatMap(f)
    Option.when(values.nonEmpty)(values.sum)

  def totalCharged: Option[BigDecimal]     = total(_.taxCharged)
  def totalPaid: Option[BigDecimal]        = total(_.taxPaid)
  def totalOutstanding: Option[BigDecimal] = total(_.outstanding)
