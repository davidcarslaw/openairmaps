
This app demonstrates the use of `addPolarMarkers()` in a `{shiny}` context. Users can define the specific sites they'd like to plot, as well as the specific pollutants they are interested in. As plotting a polar marker takes some time, `bslib::input_task_button()` ensures users can't interrupt the process while it is still running.
