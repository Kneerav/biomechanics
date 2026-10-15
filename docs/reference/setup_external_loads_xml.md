# Setup External Loads XML

This function sets up an XML file for external loads by modifying a
given base XML file with the specified parameters (leg, body, datafile)
and saves the updated XML file to a specified filename.

## Usage

``` r
setup_external_loads_xml(
  base_file = "external_loads_base.xml",
  leg = "Left",
  body = "calcn_l",
  datafile = "grf.mot",
  setup_filename = "external_loads.xml"
)
```

## Arguments

- base_file:

  A character string specifying the path to the base XML file. Default
  is "external_loads_base.xml".

- leg:

  A character string specifying the leg (e.g., "Left" or "Right").
  Default is "Left".

- body:

  A character string specifying the body part (e.g., "calcn_l"). Default
  is "calcn_l".

- datafile:

  A character string specifying the name of the data file. Default is
  "grf.mot".

- setup_filename:

  A character string specifying the name of the output setup XML file.
  Default is "external_loads.xml".

## Value

The function does not return a value. It writes the modified XML to the
specified setup_filename.
