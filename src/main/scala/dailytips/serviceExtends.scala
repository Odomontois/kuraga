package dailytips.serviceExtends

import dailytips.serviceExtends.InvoiceService.{Invoice, InvoiceId}

import java.util.UUID

trait IO[+A]

trait InvoiceService:
  extension (invoiceId: InvoiceId)
    def get: IO[Invoice]
    def update(invoice: Invoice): IO[Unit]

  // only InvoiceService implementations know that an InvoiceId is an UUID
  // all other things should rely on InvoiceService to work with InvoiceId
  protected given (InvoiceId <:< UUID) = summon

object InvoiceService:
  opaque type InvoiceId = UUID

  case class Invoice(price: BigDecimal, text: String) derives Codec

class DBInvoiceService(using db: DB) extends InvoiceService :
  extension (invoiceId: InvoiceId)
    def get: IO[Invoice] = db.get(invoiceId)
    def update(invoice: Invoice): IO[Unit] = db.put(invoiceId, invoice)

trait Codec[A]

object Codec:
  def derived[A]: Codec[A] = new {}

trait DB:
  def get[R: Codec](id: UUID): IO[R]
  def put[R: Codec](id: UUID, data: R): IO[Unit]


