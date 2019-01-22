module Day6.Steppable where


class Steppable a where
    nextSteps :: a-> [a]   
    -- nextSteps' = observer "Stepping" nextSteps 