package playground.chat.bidsell

enum TradeSide:
  case Buy, Sell

opaque type ExchangeRate <: BigDecimal = BigDecimal

case class ExchangeProjection(rate: ExchangeRate, side: TradeSide)
