-- -----------------------------------------------------------------------------
-- Day 1: The Tyranny of the Rocket Equation
-- -----------------------------------------------------------------------------

-- ------------------------------------------------
-- Part 1
-- ------------------------------------------------

-- Fuel required to launch a given module is based on its mass. 
-- 
-- Specifically, to find the fuel required for a module, take its mass, 
-- divide by three, round down, and subtract 2. 
-- 
-- Find the sum of the fuel requirements for all of the modules on the rocket.

-- | Calculates the fuel required for a given mass.
moduleFuel :: Integer -> Integer
moduleFuel mass = subtract 2 (div mass 3)

-- | Calculates the total fuel required for a list of masses.
totalFuel :: [Integer] -> Integer
totalFuel []     = 0
totalFuel (x:xs) = totalFuel xs + moduleFuel x

-- ------------------------------------------------
-- Part 2
-- ------------------------------------------------

-- You forgot to include additional fuel for the fuel you just added.
--
-- Fuel itself requires fuel just like a module - take its mass, divide by three, 
-- round down, and subtract 2. However, the fuel also requires fuel, and that 
-- fuel requires fuel, and so on.
--
-- Any mass that would require negative fuel should instead be treated as if it 
-- requires zero fuel.
-- 
-- Find the sum of the fuel requirements for all modules and fuel on the rocket.

-- | Calculates the recursive total fuel required for a list of masses.
recFuel :: [Integer] -> [Integer]
recFuel (-2:xs) = xs
recFuel (-1:xs) = xs
recFuel ( 0:xs) = xs
recFuel ( x:[]) = recFuel ([(moduleFuel x), 0]) -- replaces original with 0.
recFuel xs      = recFuel ((moduleFuel (head xs)):xs)

-- | Calculates the total recursive fuel required for a list of masses.
totalRecFuel :: [Integer] -> Integer
totalRecFuel []     = 0
totalRecFuel (x:xs) = totalRecFuel xs + sum (recFuel [x])
