SELECT 
    u.id,
    u.display_name as name,
    u.reputation, p.id, p.answer_count, p.comment_count, p.score, p.view_count, p.tags
FROM users u
	LEFT JOIN posts p 
		ON p.owner_user_id = u.id
	WHERE p.post_type_id = 1
	GROUP BY
		u.id, u.display_name, u.reputation, p.id
ORDER BY
	u.reputation DESC

	