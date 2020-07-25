## Testing

Currently the URLs for the Rest API are hardcoded strings. You will need to update these to the URL you want to test against, and potentially a legacy API URL that you want to compare with for regression tests. 

```
nix-shell shell.nix
make                     # Run all tests
make regression-suite    # Run all regression tests
make checkschema         # Run all schema tests
```

Note: This is a WIP, schemas are currently mostly faulty. Regression tests are ran against changing data and failures should be treated with skepticism - look at the test output and consider re-running. 