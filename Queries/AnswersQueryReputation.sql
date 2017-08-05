SELECT 
	COALESCE (u.id) user_id,
	COALESCE (u.display_name) user_name,
	COALESCE (u.reputation) user_reputation,
	COALESCE (q.id) answer_id,
	CASE 
		WHEN p.id=q.accepted_answer_id THEN 'YES'
		ELSE 'NO'
	END AS ACCEPT,
	p.comment_count, 
	p.score, 
	p.view_count, 
	q.tags, 
	q.owner_user_id
FROM users u 
	LEFT JOIN posts p
		ON u.id = p.owner_user_id
	LEFT JOIN posts q 
		ON p.parent_id = q.id
		WHERE p.post_type_id = 2
GROUP BY u.id, u.display_name, u.reputation, q.id, p.id
ORDER BY
	u.reputation DESC
LIMIT 1000
