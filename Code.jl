# Julia code for data wrangling

# Start by importing packages
using DataFrames
using CSV #v0.3.1

# Changing working directory to "Donnees"
cd("Donnees")

# Verifying data to collect (bnvd)
filter(s -> occursin(r"^bnvd-vp-", s), readdir()) 
# Collecting data into one dataframe
bnvd = reduce(vcat,  
    map(x -> CSV.read(x, delim = ";"), 
        filter(s -> occursin(r"^bnvd-vp-", s), readdir())
        )
    )
# Change directory
cd(".."); cd("Donnees_ref")
# Write final database
CSV.write("bnvd-vp.csv", bnvd)
bnvd = nothing

# Repeat procedure for vines data
cd(".."); cd("Donnees")

# Verifying data to collect (vin)
filter(s -> occursin(r"^vin-p-", s), readdir())
# Collecting data
## Support function
function read_as_strings(x)
    y = CSV.read(x, 
        delim = ";", 
        types = fill(Union{String, Missing}, 
        ncol(CSV.read(x, delim = ";", rows = 1)))
    )
    return y
end
## Extraction
vin = reduce((x, y) -> join(x, y, kind = :outer, on = intersect(names(x), names(y))),  
    map(x -> read_as_strings(x), 
        filter(s -> occursin(r"^vin-p-", s), readdir())
        )
    )
# Change directory
cd(".."); cd("Donnees_ref")
# Write final database
CSV.write("vin-p.csv", vin)
vin = nothing

