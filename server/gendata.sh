hostport=${1:-localhost:8080}
points=${2:-1}
reporters=${3:-1}
tsid=${4:-es.mrjon.temperatureFMillis}

echo "Generating $points points, from $reporters reporter -> http://$hostport."

for (( x=0; x<${points}; x++ ))
do
    ts=$(date -d "$x minutes ago" +%s)
    val=$((10000+${x}))
    rid=$((${x} % ${reporters}))
    curl "http://${hostport}/report?t_sec=${ts}&v=${val}&tsname=${tsid}&rid=${rid}"
done

echo "Done."


