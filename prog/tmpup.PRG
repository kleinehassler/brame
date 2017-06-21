update documents set subconiva=round((ivavalor/0.12 + descuconiva)::numeric,2) where poriva=10;
update documents set recargo=montotota-ivavalor-(subconiva-descuconiva) where poriva=10;
update documents set poriva=12 where poriva=10;