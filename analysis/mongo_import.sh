ls *.raw.json | xargs -n 1 \
  mongoimport --sslAllowInvalidCertificates \
              -c rawJbayB0Data \
              --uri "mongodb://BXD_SILICON_rw:s6mM51u1kUi1sQc@d1fm1smon010.amr.corp.intel.com:7079,d2fm1smon010.amr.corp.intel.com:7079,d3fm1smon010.amr.corp.intel.com:7079/BXD_SILICON?ssl=true&replicaSet=mongo7079" \
              --jsonArray
