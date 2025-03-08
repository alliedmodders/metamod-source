#include <string>
#include <string.h>
#include "utility.h"

bool TestTrimComments(std::string &error)
{
	char stringWithLeadingComment[32] = "// comment";
	char stringWithTrailingComment[32] = "string //";
	char stringWithNoComment[32] = "something";

	mm_TrimComments(stringWithLeadingComment);

	if (strcmp(stringWithLeadingComment, "") !=0)
	{
		error = "Failed stringWithLeadingComment - got " + std::string(stringWithLeadingComment);
		return false;
	}

	mm_TrimComments(stringWithTrailingComment);

	if (strcmp(stringWithTrailingComment, "string ") !=0)
	{
		error = "Failed stringWithTrailingComment - got " + std::string(stringWithTrailingComment);
		return false;
	}

	mm_TrimComments(stringWithNoComment);

	if (strcmp(stringWithNoComment, "something") !=0)
	{
		error = "Failed stringWithNoComment - got " + std::string(stringWithNoComment);
		return false;
	}

	return true;
}
