-- | True hourly rate math for side hustles and gig work.
--
-- A gig's posted rate is gross cash over paid hours. The real one comes
-- off the gross in platform fees and job costs first, then divides by
-- hours that include the unpaid admin around the paid ones: messaging
-- clients, invoicing, restocking, the drive back from the job. This
-- module computes that rate and compares two gigs on it.
module SideHustle.Hourly
  ( netCash
  , hourlyRate
  , compareGigs
  ) where

-- | Net cash a gig leaves after the platform cut and the costs of
-- doing the job. A cut of 0.15 means the platform keeps 15 percent.
netCash :: Double -> Double -> Double -> Double
netCash gross platformCut costs = gross * (1 - platformCut) - costs

-- | Net cash over total hours, paid and unpaid. Returns 0 when there
-- are no hours at all.
hourlyRate :: Double -> Double -> Double -> Double
hourlyRate net paidHours unpaidHours
  | paidHours + unpaidHours <= 0 = 0
  | otherwise                    = net / (paidHours + unpaidHours)

-- | The better of two hourly rates and the per-hour gap between them,
-- returned as (better rate, how far ahead it is).
compareGigs :: Double -> Double -> (Double, Double)
compareGigs rateA rateB
  | rateA >= rateB = (rateA, rateA - rateB)
  | otherwise      = (rateB, rateB - rateA)
