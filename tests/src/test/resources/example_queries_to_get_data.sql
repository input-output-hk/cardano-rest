-- Remember to copy the values out into a text file!
-- Also, you can change the limit to get more/less data.

-- It'd be a good idea to automate refreshing data over time so we don't get stale :)
\copy (select address from tx_out where random() < 0.01 limit 2500) to 'addresses.csv' csv;
\copy (select hash from block where random() < 0.01 limit 2500) to 'blocks.csv' csv;
\copy (select hash from tx where random() < 0.01 limit 2500) to 'transactions.csv' csv;
\copy (select block.hash, tx_out.address from tx_out left join tx on tx.id = tx_out.tx_id left join block on block.id = tx.block where random() < 0.01 limit 2500) to 'blocks_and_addresses.csv' csv;
