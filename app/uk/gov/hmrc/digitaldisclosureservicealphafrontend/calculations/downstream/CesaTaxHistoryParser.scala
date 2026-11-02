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

import play.api.libs.json.{JsObject, JsValue}
import uk.gov.hmrc.digitaldisclosureservicealphafrontend.calculations.model.*

import java.time.LocalDate
import scala.collection.immutable.ListMap
import scala.util.Try

/** Turns CESA responses into a [[CesaTaxHistory]] keyed by the calculation's tax years and income types. */
object CesaTaxHistoryParser:

  /** CESA return pages -> the calculation's `incomeTypes` values (question-pack-full.json). */
  val ScheduleToIncomeType: ListMap[String, String] = ListMap(
    "employment"             -> "employment",
    "self_employment"        -> "selfEmployment",
    "uk_property"            -> "ukProperty",
    "partnership"            -> "partnership",
    "foreign"                -> "foreignIncome",
    "trusts_and_settlements" -> "trustsEstates",
    "capital_gains_summary"  -> "capitalGains"
  )

  /** Message keys for the income types above (the same labels the calculation's checkboxes use). */
  val IncomeTypeLabels: Map[String, String] = Map(
    "employment"     -> "Employment_income",
    "selfEmployment" -> "Self_employment_income",
    "ukProperty"     -> "Income_from_letting_out_property",
    "partnership"    -> "Partnership_income",
    "foreignIncome"  -> "Foreign_income_or_dividends",
    "trustsEstates"  -> "Income_from_a_trust",
    "capitalGains"   -> "Capital_gains"
  )

  /** Pages that make up the return itself rather than an income source. */
  private val CoreForms = Set(
    "short_tax_return",
    "individual_tax_form",
    "tax_calculation_summary",
    "additional_information",
    "repayment_to_charity_flyer"
  )

  /** The calculation's label for the tax year starting 6 April of `startYear`, e.g. 2016 -> "2016-17". */
  def label(startYear: Int): String = f"$startYear-${(startYear + 1) % 100}%02d"

  /** A CESA date (usually the tax year end, "2017-04-05") -> "2016-17". A date on or after 6 April starts that year. */
  def taxYearFromDate(date: String): Option[String] =
    Try(LocalDate.parse(date.take(10))).toOption.map: d =>
      label(if d.isBefore(LocalDate.of(d.getYear, 4, 6)) then d.getYear - 1 else d.getYear)

  /** CESA's short form, "1617" -> "2016-17". */
  def taxYearFromShort(code: String): Option[String] =
    if code.matches("\\d{4}") then Some(label(2000 + code.take(2).toInt)) else None

  /** "2016-17" -> 2017, the year-end CESA uses in `/return/{year}`. */
  def taxYearEnd(taxYear: String): Option[Int] =
    Try(taxYear.take(4).toInt + 1).toOption

  def calendarYears(json: JsValue): Seq[CesaTaxYear] =
    (json \ "returnYears").asOpt[Seq[JsObject]].getOrElse(Nil).flatMap: year =>
      (year \ "taxYearEnd").asOpt[String].flatMap(taxYearFromDate).map: taxYear =>
        CesaTaxYear(
          taxYear            = taxYear,
          inCalendar         = true,
          returnIssueDate    = string(year, "returnIssueDate"),
          filingDueDate      = string(year, "onlineDueDate"),
          returnReceivedDate = string(year, "returnReceivedDate")
        )

  /** (calculation income types declared, other non-core pages) from a `/return/{year}` response. */
  def schedules(json: JsValue): (Seq[String], Seq[String]) =
    val all = (json \ "supplementarySchedules").asOpt[Seq[String]].getOrElse(Nil)
    (all.flatMap(ScheduleToIncomeType.get).distinct, all.filterNot(s => ScheduleToIncomeType.contains(s) || CoreForms(s)))

  /** Per-year declared income, tax charged, paid, outstanding and payments from `/account/tax-years`. */
  def accountYears(json: JsValue): Map[String, CesaTaxYear] =
    (json \ "taxYears")
      .asOpt[Seq[JsObject]]
      .getOrElse(Nil)
      .flatMap: year =>
        (year \ "taxYear").asOpt[String].flatMap(taxYearFromShort).map: taxYear =>
          taxYear -> CesaTaxYear(
            taxYear        = taxYear,
            declaredIncome = money(year, "declaredIncome"),
            taxCharged     = money(year, "taxCharged"),
            taxPaid        = money(year, "taxPaid"),
            outstanding    = money(year, "outstanding"),
            payments       = (year \ "payments").asOpt[Seq[JsObject]].getOrElse(Nil).flatMap { p =>
              money(p, "amount").map(CesaPayment(string(p, "date"), _, string(p, "description")))
            }
          )
      .toMap

  /** Enquiries and No Repayment Signals from `/signals`, by tax year (signals with no tax year are returned separately). */
  def signals(json: JsValue): (Map[String, Seq[CesaEnquiry]], Map[String, Seq[RepaymentSignal]], Seq[RepaymentSignal]) =
    val enquiries =
      (json \ "enquiries").asOpt[Seq[JsObject]].getOrElse(Nil).flatMap: e =>
        (e \ "taxYear").asOpt[String].flatMap(taxYearFromShort).map: taxYear =>
          taxYear -> CesaEnquiry(
            enquiryType       = string(e, "type"),
            open              = string(e, "status").exists(_.equalsIgnoreCase("OPEN")),
            openedDate        = string(e, "openedDate"),
            closedDate        = string(e, "closedDate"),
            lastDateOfEnquiry = string(e, "lastDateOfEnquiry")
          )
    val allSignals =
      (json \ "noRepaymentSignals").asOpt[Seq[JsObject]].getOrElse(Nil).flatMap: s =>
        (s \ "code").asOpt[Int].map: code =>
          RepaymentSignal(
            code       = code,
            taxYear    = (s \ "taxYear").asOpt[String].flatMap(taxYearFromShort),
            reason     = string(s, "reason").getOrElse(s"No Repayment Signal $code"),
            department = string(s, "department"),
            setDate    = string(s, "setDate")
          )
    (
      enquiries.groupMap(_._1)(_._2),
      allSignals.collect { case s if s.taxYear.isDefined => s.taxYear.get -> s }.groupMap(_._1)(_._2),
      allSignals.filter(_.taxYear.isEmpty)
    )

  def tradeName(json: JsValue): Option[String] =
    (json \ "tradeName").asOpt[String].map(_.trim).filter(_.nonEmpty)

  def taxpayerName(json: JsValue): Option[String] =
    val name  = (json \ "name").asOpt[JsObject].getOrElse(JsObject.empty)
    val parts = Seq("title", "forename", "surname").flatMap(string(name, _))
    Option.when(parts.nonEmpty)(parts.mkString(" "))

  def build(
    utr     : String,
    source  : String,
    calendar: Option[JsValue] = None,
    returns : Map[String, JsValue] = Map.empty,
    account : Option[JsValue] = None,
    signalsJson: Option[JsValue] = None,
    trade   : Option[JsValue] = None,
    taxpayer: Option[JsValue] = None
  ): CesaTaxHistory =
    val fromCalendar = calendar.map(calendarYears).getOrElse(Nil)
    val fromAccount  = account.map(accountYears).getOrElse(Map.empty)
    val (enquiriesByYear, signalsByYear, otherSignals) =
      signalsJson.map(signals).getOrElse((Map.empty, Map.empty, Nil))

    val allYears =
      (fromCalendar.map(_.taxYear) ++ returns.keys ++ fromAccount.keys ++ enquiriesByYear.keys ++ signalsByYear.keys).distinct.sorted.reverse

    val years = allYears.map: taxYear =>
      val base              = fromCalendar.find(_.taxYear == taxYear).getOrElse(CesaTaxYear(taxYear))
      val acct              = fromAccount.get(taxYear)
      val (declared, other) = returns.get(taxYear).map(schedules).getOrElse((Nil, Nil))
      base.copy(
        declaredIncomeTypes = declared,
        otherSchedules      = other,
        declaredIncome      = acct.flatMap(_.declaredIncome),
        taxCharged          = acct.flatMap(_.taxCharged),
        taxPaid             = acct.flatMap(_.taxPaid),
        outstanding         = acct.flatMap(_.outstanding),
        payments            = acct.map(_.payments).getOrElse(Nil),
        enquiries           = enquiriesByYear.getOrElse(taxYear, Nil),
        repaymentSignals    = signalsByYear.getOrElse(taxYear, Nil)
      )

    CesaTaxHistory(
      utr          = utr,
      source       = source,
      taxpayerName = taxpayer.flatMap(taxpayerName),
      tradeName    = trade.flatMap(tradeName),
      years        = years,
      otherSignals = otherSignals
    )

  private def string(json: JsObject, field: String): Option[String] =
    (json \ field).asOpt[String].map(_.trim).filter(_.nonEmpty)

  /** CESA money: `{"amount": 123.45, "currency": "GBP"}` (null or missing gives None). */
  private def money(json: JsObject, field: String): Option[BigDecimal] =
    (json \ field \ "amount").asOpt[BigDecimal]
